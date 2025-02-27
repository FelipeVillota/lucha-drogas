# library(shiny)
# library(leaflet)
# library(ggplot2)
# library(plotly)
# library(DT)
# library(tidyverse)
# library(lubridate)
# 
# source("descripciones-datasets-app.R")
# 
# 
# ui <- fluidPage(
#   tags$head(
#     tags$style(HTML("body { background-color: #121212; color: #ffffff; }
#                       .title-container { text-align: center; padding: 20px; }
#                       h2 { font-size: 28px; font-weight: bold; }
#                       p { font-size: 16px; max-width: 800px; margin: 0 auto; }
#                       .sidebar-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
#                       .main-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
#                    "))
#   ),
#   fluidRow(
#     div(class = "title-container",
#         h2("Colombia vs Drogas"),
#         p(HTML("Integración y exploración espacio-temporal de todos los datos disponibles relativos a los avances en la lucha contra el problema mundial de las drogas del
#                 <a href='https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica'
#                 target='_blank' style='color: #007bff; text-decoration: none; font-weight: bold;'>Ministerio de Defensa</a>
#                 (versión publicada 16 de enero de 2025)"))
#     )
#   ),
#   sidebarLayout(
#     sidebarPanel(class = "sidebar-panel",
#                  selectInput("dataset", "Campos de acción:", choices = names(working_data)),
#                  div(style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
#                      htmlOutput("dataset_description")
#                  )
#     ),
#     mainPanel(class = "main-panel",
#               leafletOutput("map", height = "60vh"),
#               plotlyOutput("timeline", height = "30vh")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   selected_data <- reactive({
#     req(input$dataset)
#     working_data[[input$dataset]]
#   })
# 
#   output$dataset_description <- renderUI({
#     desc <- dataset_descriptions[[input$dataset]]
#     HTML(paste0("<strong>Descripción:</strong> ", desc))
#   })
# 
#   output$map <- renderLeaflet({
#     df <- selected_data()
# 
#     validate(
#       need(all(c("LATITUD", "LONGITUD") %in% colnames(df)),
#            "El conjunto seleccionado no contiene coordenadas válidas (LATITUD/LONGITUD)")
#     )
# 
#     leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
#       addProviderTiles(providers$Stadia.StamenToner) %>%
#       addMarkers(
#         lng = ~LONGITUD,
#         lat = ~LATITUD,
#         clusterOptions = markerClusterOptions(),
#         icon = makeIcon(iconWidth = 8, iconHeight = 8)
#       ) %>%
#       setView(lng = median(df$LONGITUD, na.rm = TRUE),
#               lat = median(df$LATITUD, na.rm = TRUE),
#               zoom = ifelse(nrow(df) > 1000, 5, 7))
#   })
# 
#   output$timeline <- renderPlotly({
#     df <- selected_data()
#     
#     validate(
#       need("fecha_hecho" %in% colnames(df) & "cantidad" %in% colnames(df) & "municipio" %in% colnames(df),
#            "El conjunto seleccionado no contiene datos válidos para la línea de tiempo")
#     )
#     
#     df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
#     
#     p <- ggplot(df, aes(x = fecha_hecho, y = cantidad, text = paste("Fecha:", fecha_hecho, "<br>Municipio:", municipio, "<br>Cantidad:", cantidad))) +
#       geom_point(size = 1, color = "#00BFC4", alpha = 0.5) +
#       labs(x = "Fecha", y = "Cantidad", title = "Cantidad reportada a lo largo del tiempo") +
#       theme_minimal() +
#       theme(plot.background = element_rect(fill = "#1e1e1e"),
#             panel.background = element_rect(fill = "#1e1e1e"),
#             text = element_text(color = "#ffffff"),
#             axis.text = element_text(color = "#ffffff"),
#             axis.title = element_text(color = "#ffffff"))
#     
#     ggplotly(p, tooltip = "text")
# 
# 
# 
#   })
# }
# 
# shinyApp(ui, server) ## la buena versión
# 
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
    tags$head(
      tags$style(HTML("body { background-color: #121212; color: #ffffff; }
                        .title-container { text-align: center; padding: 20px; }
                        h2 { font-size: 28px; font-weight: bold; }
                        p { font-size: 16px; max-width: 800px; margin: 0 auto; }
                        .sidebar-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
                        .main-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
                     "))
    ),
    fluidRow(
      div(class = "title-container",
          h2("Colombia vs Drogas"),
          p(HTML("Integración y exploración espacio-temporal de todos los datos disponibles relativos a los avances en la lucha contra el problema mundial de las drogas del
                  <a href='https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica'
                  target='_blank' style='color: #007bff; text-decoration: none; font-weight: bold;'>Ministerio de Defensa</a>
                  (versión publicada 16 de enero de 2025)"))
      )
    ),
    sidebarLayout(
      sidebarPanel(class = "sidebar-panel",
                   selectInput("dataset", "Campos de acción:", choices = names(working_data)),
                   div(style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
                       htmlOutput("dataset_description")
                   )
      ),
      mainPanel(class = "main-panel",
                leafletOutput("map", height = "60vh"),
                plotlyOutput("timeline", height = "30vh")
      )
    )
  )

server <- function(input, output) {
  selected_data <- reactive({
    req(input$dataset)
    df <- working_data[[input$dataset]]
    
    validate(
      need(all(c("LATITUD", "LONGITUD") %in% colnames(df)), "El conjunto seleccionado no contiene coordenadas válidas")
    )
    
    df
  })
  
  output$dataset_description <- renderUI({
        desc <- dataset_descriptions[[input$dataset]]
        HTML(paste0("<strong>Descripción:</strong> ", desc))
  
  })

  
  output$map <- renderLeaflet({
    df <- selected_data()
    
    leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~LONGITUD,
        lat = ~LATITUD,
        radius = 3,
        color = "#FF5733",
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = median(df$LONGITUD, na.rm = TRUE), 
              lat = median(df$LATITUD, na.rm = TRUE), 
              zoom = ifelse(nrow(df) > 1000, 5, 7))
  })
  
  output$timeline <- renderPlotly({
    df <- selected_data()
    
    validate(
      need(all(c("fecha_hecho", "cantidad") %in% colnames(df)), "El conjunto seleccionado no contiene datos válidos para la línea de tiempo")
    )
    
    df <- df %>%
      mutate(fecha_hecho = as.Date(fecha_hecho)) %>%
      group_by(fecha_hecho) %>%
      summarize(cantidad = sum(cantidad), .groups = "drop")
    
    p <- ggplot(df, aes(x = fecha_hecho, y = cantidad)) +
      geom_line(color = "#00BFC4", size = 0.8) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.background = element_rect(fill = "#1e1e1e"),
        text = element_text(color = "#ffffff"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff")
      )
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
