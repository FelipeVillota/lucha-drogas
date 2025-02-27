library(shiny)
library(leaflet)
library(DT)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Geographic Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", 
                  "Choose dataset:", 
                  choices = names(working_data))
    ),
    mainPanel(
      leafletOutput("map"),
      h4("Data Summary"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    req(input$dataset)
    working_data[[input$dataset]]
  })
  
  output$map <- renderLeaflet({
    df <- selected_data()
    
    validate(
      need(all(c("LATITUD", "LONGITUD") %in% colnames(df)),
           "Selected dataset does not contain required LATITUDE and LONGITUDE columns")
    )
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUD,
        lat = ~LATITUD,
        radius = 5,
        color = "blue",
        fillOpacity = 0.8
      )
  })
  
  output$table <- renderDT({
    df <- selected_data()
    datatable(head(df),
              options = list(scrollX = TRUE, pageLength = 5))
  })
}

shinyApp(ui, server)
