library(shiny)
library(shinyWidgets)
library(ggiraph)
library(tidyverse)

myGreys <- readRDS("myGreys.RDS")

DT <- readRDS("percentiles.RDS")
dtObs <- readRDS("percentilesObs.RDS")

p <- DT$percentile %>% unique

regions <- DT %>% pull(region) %>% levels

myTheme2 <- function() {
    
    theme_minimal(base_size = 16) +
        theme(text = element_text(colour = myGreys[[5]]),
              axis.text = element_text(colour = myGreys[[8]]),
              panel.grid = element_line(colour = myGreys[[9]], linetype = "dotted", size = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5),
              strip.text = element_text(face = "bold", colour = myGreys[[5]]),
              panel.spacing = unit(0.5, "cm"),
              legend.position = "top",
              title = element_text(colour = myGreys[[5]]),
              legend.title = element_text(colour = myGreys[[5]]),
              legend.text = element_text(margin = margin(r = 10, unit = "pt")),
              legend.text.align = 0)
    
}

ui <- fluidPage(
    
    titlePanel("CRM Validation"),
    
    sidebarLayout(
        sidebarPanel(width = 2,
                     radioGroupButtons(inputId = "id_ensemble", label = "Select ensemble:", choices = c("MME", "PPE"), selected = "MME", direction = "vertical"),
                     radioGroupButtons(inputId = "id_region", label = "Select region:", choices = regions, selected = "Highlands", direction = "vertical"),
                     sliderTextInput(inputId = "id_slider", label = "Select percentiles:", choices = p, selected = c(5,75), force_edges = TRUE),
                     materialSwitch(inputId = "id_sim", label = strong(paste("Show simulated flow before bias correction:")), value = TRUE)
        ),
        mainPanel(girafeOutput("plot", width = "100%", height = "100%"))
    )
)


server <- function(input, output) {
    
    data <- reactive({ 
        DT %>% 
            filter(percentile %in% input$id_slider) %>% 
            filter(region == input$id_region & ensemble == input$id_ensemble) 
    })
    
    dataObs <- reactive({ 
        dtObs %>% 
            filter(p %in% input$id_slider) %>% 
            filter(region == input$id_region & ensemble == input$id_ensemble) 
    })
    
    output$plot <- renderGirafe({
        
        plot <- 
            data() %>%
            ggplot(aes(x = percentile, y = med, ymin = min, ymax = max, fill = variable, colour = variable, group = stationID))
        
        if(input$id_sim == TRUE) { plot <- plot + geom_ribbon(data = . %>% filter(variable == "sim")) }
        
        plot <- 
            plot +
            geom_ribbon(data = . %>% filter(variable == "simBC")) +
            
            geom_line(data = dataObs(), aes(x = p, y = value, colour = variable, fill = variable, group = stationID),
                      inherit.aes = FALSE, size = 0.75, linetype = 2) +
            
            facet_wrap(~stationID, scales = "free_y") +
            
            myTheme2() +
            theme(legend.title = element_blank(),
                  panel.grid.minor = element_blank()) +
            
            scale_fill_manual(values = c(viridis::viridis(option = "B", n = 1, begin = 0.75, end = 0.8), myGreys[[9]], myGreys[[5]]),
                              labels = c("Observed", "Simulated", "Simulated - Bias corrected"), drop = FALSE) +
            scale_colour_manual(values = c(viridis::viridis(option = "B", n = 1, begin = 0.75, end = 0.8), myGreys[[9]], myGreys[[5]]), drop = FALSE) +
            scale_x_continuous(breaks = p, minor_breaks = NULL, expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) +
            guides(colour = FALSE) +
            
            labs(x = "Percentile", y = "Flow\n(cubic metres per second)")
        
        girafe(ggobj = plot, options = list(opts_sizing(rescale = FALSE)), width_svg = 20, height_svg = 12)
        
        
    })
    
}

shinyApp(ui = ui, server = server)