#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RCurl)
library(tidyverse)
library(leaflet)


resorts.git <- getURL("https://raw.githubusercontent.com/niedermansam/SkiResortApp/master/skiResorts_geocoded3.csv")
resorts <- read.csv(text = resorts.git) %>% as.tibble()

resorts$label <- sprintf("<div style='text-align:center'><a href='%s' target='_blank'><strong>%s</strong></a><br/> %s %s %s %s</div>",
                         resorts$url,resorts$name, 
                         ifelse(!is.na(resorts$vertical), paste0("<br/>Vertical Rise: <strong>",resorts$vertical," ft.</strong>"),""),
                         ifelse(!is.na(resorts$acres), paste0("<br/>Skiable Acres: <strong>",resorts$acres," acres</strong>"),""),
                         ifelse(!is.na(resorts$lifts), paste0("<br/>Number of Lifts: <strong>",resorts$lifts," lifts</strong>"),""),
                         ifelse(!is.na(resorts$ticket), paste0("<br/><br/><strong>Ticket Price: $",resorts$ticket,"</strong>"),"")) %>% lapply(htmltools::HTML)


resorts_ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "price", 
                            label = "Max Ticket Price:", 
                            min = 0, max = 200, value = 200, step = 1),
                sliderInput(inputId = "vert", 
                            label = "Minimum Vertical Rise:", 
                            min = 0, max = 6000, value = 0, step = 100),
                radioButtons("base_map", "Base Map:",
                             c("Stamen Terrain" = "Stamen.Terrain",
                               "Open Street Map" = "OpenStreetMap",
                               "Open Topo Map" = "OpenTopoMap")),
                plotOutput("histPrice",height = 200),
                plotOutput("scatterPriceVert",height = 250)
  )
)

resorts_server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = -100, lat = 50, zoom = 4)
  })
  
  
  
  # A reactive expression that returns the set of resorts that are
  # in bounds right now
  resortInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(resorts[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    max_price <- input$price
    min_vert <- input$vert
    
    subset(resorts,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2] &
             ticket <= max_price & vertical >= min_vert)
  })
  
  
  ## Make Ticket Prices Histogram
  priceBreaks <- hist(plot = FALSE, resorts$ticket, breaks = 20)$breaks
  
  output$histPrice <- renderPlot({
    # If no resorts are in view, don't plot
    if (nrow(resortInBounds()) == 0)
      return(NULL)
    
    print(ggplot(resortInBounds(), aes(x=ticket)) +
            geom_histogram(fill = "lightblue", color="black", bins = 30) +
            labs(title = "Histogram of Ticket Prices",
                 subtitle = "Resorts in View",
                 x = "Ticket Price") + 
      theme(plot.title = element_text(hjust = .5,size=16),
            plot.subtitle = element_text(hjust = .5,size=14),
            plot.background = element_rect(color = "black"), 
            axis.title = element_text(size=14),
            plot.margin = margin(10,20,10,10)))
  })
  
  output$scatterPriceVert <- renderPlot({
    # If no resorts are in view, don't plot
    if (nrow(resortInBounds()) == 0)
      return(NULL)
    
    print(ggplot(resortInBounds(),
                 aes(y = acres, x = vertical,color = ticket)) + 
            geom_point(size=2) + 
            theme_grey() +
            scale_x_continuous(trans='log2', 
                               breaks = c(100,200,400,800,1600,3200,6400,12800), 
                               minor_breaks = NULL) + 
            scale_y_continuous(trans='log2', 
                               breaks = c(3,6,12,25,50,100,200,400,800,1600,3200,6400),
                               minor_breaks = NULL) + 
            labs(title = "Ski Resort Acres vs. Vertical Feet", 
                 subtitle = "Colored by Ticket Price", 
                 y = "Skiable Acres",
                 x =" Vertical Rise") + 
            theme(plot.title = element_text(hjust = .5,size=16),
                  plot.subtitle = element_text(hjust = .5,size=14),
                  plot.background = element_rect(color = "black"), 
                  axis.title = element_text(size=14),
                  plot.margin = margin(10,1,10,10)) + 
            labs(color = "Price"))
  })
  
  
  observe({
    
    max_price <- input$price
    min_vert <- input$vert
    base_map <- input$base_map
    
    sites <- resorts %>% 
      filter(ticket <= max_price & vertical > min_vert)
    
    leafletProxy("map") %>% clearMarkers() %>% 
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 popup = sites$label)
  })
  
}

shinyApp(resorts_ui,resorts_server)

