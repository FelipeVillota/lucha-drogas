# COLOMBIA VS DROGAS - DATA VISUALIZATION APP
#
# Author: Luis Felipe Villota Macías
# Description: Shiny application for visualizing drug intervention data in Colombia

# Libraries ----
library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyjs)

source("output-descripciones.R")

# UI ----
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "body { background-color: #121212; color: #ffffff; }
        .title-container { text-align: center; padding: 20px; }
        h2 { font-size: 28px; font-weight: bold; }
        p { font-size: 16px; max-width: 800px; margin: 0 auto; }
        .sidebar-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
        .main-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
        .time-control { background-color: #2a2a2a; padding: 10px; border-radius: 5px; margin-top: 10px; }
        .loading-spinner {
          display: inline-block;
          width: auto;
          text-align: center;
          padding: 10px 20px;
          color: white;
          background-color: rgba(0,0,0,0.7);
          border-radius: 5px;
        }
        .leaflet-control-minimap {
          border: 2px solid rgba(255, 255, 255, 0.5) !important;
        }
        .search-panel {
          background-color: #2a2a2a;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 15px;
        }
        .highlight { background-color: #4a4a4a !important; }
        "
      )
    ),
    tags$script(HTML("
      $(document).on('shiny:busy', function() {
        $('.loading-indicator').show();
      });

      $(document).on('shiny:idle', function() {
        $('.loading-indicator').hide();
      });

      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('appReady', true);
      });
    "))
  ),
  fluidRow(
    div(
      class = "title-container",
      h2("Colombia vs Drogas"),
      p(
        HTML(
          "Integración y exploración espacio-temporal de todos los datos disponibles relativos a los avances en la lucha contra el problema mundial de las drogas del
          <a href='https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica'
          target='_blank' style='color: #007bff; text-decoration: none; font-weight: bold;'>Ministerio de Defensa</a>
          (versión publicada 16 de enero de 2025)"
        )
      ), 
      p("Autor: Luis Felipe Villota Macías", style = "font-size: 14px; color: #888; margin-top: 10px;") 
    )
  ),

  ## Sidebar layout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("dataset", "Tipo de operación:", choices = names(working_data)),

      ### Dataset description ----
      div(
        style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
        htmlOutput("dataset_description")
      ),

      ### Map Configuration ----
      div(
        class = "time-control",
        h4("Configuración del mapa"),
        sliderInput(
          "heatIntensity",
          "Intensidad:",
          min = 0.1,
          max = 2.0,
          value = 0.2,
          step = 0.1,
          animate = FALSE
        ),
        sliderInput(
          "heatRadius",
          "Radio:",
          min = 5,
          max = 20,
          value = 8,
          step = 1,
          animate = FALSE
        )
      ),

      ### Loading Status ----
      div(
        id = "loading-status",
        class = "loading-indicator",
        style = "display: none; margin-top: 15px; text-align: center;",
        tags$div(
          class = "loading-spinner",
          "Cargando datos..."
        )
      )
    ),

    ### Main panel ----
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "mainTabs",

        #### Tab 1 ----
        tabPanel(
          "Mapa de calor de cantidades intervenidas",
          value = "map_tab",
          div(
            style = "position:relative;",
            leafletOutput("map", height = "60vh"),
            div(
              id = "loading-map",
              class = "loading-indicator",
              style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
              tags$div(
                class = "loading-spinner",
                "Cargando mapa..."
              )
            ),
            absolutePanel(
              bottom = 10,
              left = 10,
              style = paste0(
                "z-index:500; background-color: rgba(240,240,240,0.8); padding: 8px; border-radius: 5px;",
                "box-shadow: 0 0 15px rgba(0,0,0,0.2); color: black;"
              ),
              checkboxInput("showClusters", tags$span("Número de operaciones (clusters municipales)", style = "color: black;"), value = FALSE, width = "100%")
            )
          ),
          div(
            style = "position:relative; margin-top: 10px;",
            plotlyOutput("timeline", height = "30vh"),
            div(
              id = "loading-timeline",
              class = "loading-indicator",
              style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
              tags$div(
                class = "loading-spinner",
                "Cargando línea de tiempo..."
              )
            )
          )
        ),

        #### Tab 2 ----
        tabPanel(
          "Distribución temporal (por municipio)",
          value = "heatmap_tab",
          div(
            style = "position:relative; height: 80vh;",
            fluidRow(
              column(8,
                     plotlyOutput("heatmapTime", height = "70vh")),
              column(4,
                     div(class = "search-panel",
                         selectizeInput("municipio_search", "Busca y compara municipios:",
                                        choices = NULL, multiple = TRUE,
                                        options = list(
                                          placeholder = 'Escribe',
                                          maxOptions = 1000,
                                          render = I("{option: function(item, escape) {
                                              return '<div style=\"color: #000;\">' + escape(item.value) + '</div>';
                                            }}")
                                        )),
                         actionButton("reset_search", "Mostrar Todos",
                                      class = "btn-block",
                                      style = "margin-top: 10px;")
                     ),
                     plotlyOutput("selected_timeline", height = "25vh")
              )
            )
          ),
          p("Use el buscador para filtrar municipios específicos o explore la vista general")
        )
      )
    )
  )
)

## Server ----
server <- function(input, output, session) {

  ### Reactive values ----
  dataset_cache <- reactiveVal(list())
  processing_status <- reactiveVal("idle")
  municipio_choices <- reactiveVal(NULL)
  selected_municipios <- reactiveVal(NULL)

  ### Selected data ----
  selected_data <- reactive({
    req(input$dataset)
    processing_status("loading")

    cached_data <- dataset_cache()[[input$dataset]]
    if (!is.null(cached_data)) {
      processing_status("idle")
      return(cached_data)
    }

    df <- working_data[[input$dataset]] %>%
      filter(
        !is.na(LATITUD),
        !is.na(LONGITUD),
        between(LATITUD, -4.23, 13.5),
        between(LONGITUD, -82.0, -66.87)
      )

    validate(need(nrow(df) > 0, "El conjunto seleccionado no contiene coordenadas válidas"))

    if ("fecha_hecho" %in% colnames(df)) {
      tryCatch({
        df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
      }, error = function(e) {
        df$fecha_hecho <- as.Date(NA)
      })
    }

    current_cache <- dataset_cache()
    current_cache[[input$dataset]] <- df
    dataset_cache(current_cache)

    processing_status("idle")
    return(df)
  })

  # Map data ----
  map_data <- reactive({
    df <- selected_data()
    list(
      data = df,
      center = list(lng = -74.297333, lat = 4.570868),
      zoom = 5
    )
  })

  # Observe processing status ----
  observe({
    if (processing_status() == "loading") {
      shinyjs::show("loading-status")
    } else {
      shinyjs::hide("loading-status")
    }
  })

  # Dataset description ----
  output$dataset_description <- renderUI({
    req(input$dataset)
    HTML(paste0("<strong>Descripción:</strong> ", dataset_descriptions[[input$dataset]]))
  })


  # Map output ----
  output$map <- renderLeaflet({
    req(input$mainTabs == "map_tab")
    map_info <- map_data()
    df <- map_info$data
    validate(need(nrow(df) > 0, "No hay datos válidos para mostrar"))
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = map_info$center$lng, lat = map_info$center$lat, zoom = map_info$zoom) %>%
      addHeatmap(
        data = df,
        lng = ~ LONGITUD,
        lat = ~ LATITUD,
        intensity = if ("cantidad" %in% colnames(df)) ~ cantidad else 1,
        radius = input$heatRadius,
        blur = input$heatRadius * 1.5,
        max = input$heatIntensity,
        gradient = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000")
      ) %>%
      addMiniMap(
        tiles = providers$CartoDB.DarkMatterNoLabels,
        position = "bottomright",
        width = 150,
        height = 150,
        zoomLevelOffset = -5,
        toggleDisplay = TRUE,
        aimingRectOptions = list(
          color = "#2a788e",  # Viridis mid-blue color
          weight = 2,
          fillColor = "#440154", # Viridis dark purple
          fillOpacity = 0.3
        ),
        shadowRectOptions = list(
          color = "#65cb5e", # Viridis green-blue
          weight = 1,
          fillColor = "#65cb5e", # Viridis blue
          fillOpacity = 0.2
        )
      )
  })

  # Cluster observer ----
  observe({
    req(map_data())
    df <- map_data()$data
    map_proxy <- leafletProxy("map")
    
    # Always clear markers and cluster layers
    map_proxy %>% 
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (input$showClusters && nrow(df) > 0) {
      popup_content <- sapply(1:nrow(df), function(i) {
        paste0(
          if ("municipio" %in% colnames(df)) paste0("<b>Municipio:</b> ", df$municipio[i], "<br/>"),
          if ("fecha_hecho" %in% colnames(df)) paste0("<b>Fecha:</b> ", format(df$fecha_hecho[i], "%d-%m-%Y"), "<br/>"),
          if ("cantidad" %in% colnames(df)) paste0("<b>Cantidad:</b> ", df$cantidad[i])
        )
      })
      
      map_proxy %>% addCircleMarkers(
        data = df,
        lng = ~LONGITUD,
        lat = ~LATITUD,
        radius = 2,
        color = "#FFFFFF",
        fillOpacity = 0.5,
        popup = popup_content,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 30,
          spiderfyOnMaxZoom = TRUE,
          zoomToBoundsOnClick = TRUE
        )
      )
    }
  })
  
  # Timeline plot ----
  output$timeline <- renderPlotly({
    req(input$mainTabs == "map_tab")
    df <- selected_data() %>% 
      filter(!is.na(fecha_hecho)) %>% # Ensure fecha_hecho is not NA
      mutate(month = floor_date(fecha_hecho, "month")) %>% 
      count(month, name = "cantidad")
    
    if (nrow(df) > 0) {
      date_range <- seq(min(df$month), max(df$month), by = "month")
      df <- data.frame(month = date_range) %>% 
        left_join(df, by = "month") %>% 
        mutate(cantidad = replace_na(cantidad, 0))
      
      plot_ly(df, x = ~month, y = ~cantidad, type = 'scatter', mode = 'lines+markers', 
              marker = list(color = ~cantidad, colorscale = 'Viridis'),
              line = list(color = '#21918c')) %>%
        layout(
          title = list(text = "Evolución Temporal de Operaciones", font = list(color = '#FFFFFF')),
          xaxis = list(title = "Fecha", gridcolor = '#444444', tickformat = "%b %Y"),
          yaxis = list(title = "Número", gridcolor = '#444444'),
          plot_bgcolor = '#1e1e1e',
          paper_bgcolor = '#1e1e1e'
        )
    } else {
      # Return an empty plot if there's no data
      plotly_empty() %>% 
        layout(
          title = list(text = "No hay datos disponibles para el gráfico de línea de tiempo", font = list(color = '#FFFFFF')),
          xaxis = list(title = "Fecha", gridcolor = '#444444'),
          yaxis = list(title = "Número", gridcolor = '#444444'),
          plot_bgcolor = '#1e1e1e',
          paper_bgcolor = '#1e1e1e'
        )
    }
  })

  # Heatmap logic ----
  prepare_heatmap_data <- reactive({
    df <- selected_data()

    df %>%
      filter(!is.na(fecha_hecho)) %>%
      mutate(month = floor_date(fecha_hecho, "month"),
             municipio = as.character(municipio)) %>%
      count(month, municipio, name = "total") %>%
      filter(total > 0) %>%
      arrange(month)
  })

  observe({
    df_agg <- prepare_heatmap_data()
    updateSelectizeInput(session, "municipio_search", choices = unique(df_agg$municipio), server = TRUE)
  })

  output$heatmapTime <- renderPlotly({
    req(input$mainTabs == "heatmap_tab")

    df_agg <- prepare_heatmap_data()

    # Filter based on selected municipios, or show top 100 if none selected
    if (!is.null(input$municipio_search) && length(input$municipio_search) > 0) {
      filtered_df <- df_agg %>% filter(municipio %in% input$municipio_search)
    } else {
      top_municipios <- df_agg %>%
        group_by(municipio) %>%
        summarise(total = sum(total)) %>%
        slice_max(total, n = 100) %>%
        pull(municipio)
      filtered_df <- df_agg %>% filter(municipio %in% top_municipios)
    }

    # Create the heatmap plot
    if (nrow(filtered_df) > 0) {
      plot_ly(filtered_df, x = ~month, y = ~municipio, z = ~total, type = "heatmap",
              colorscale = "Viridis", hoverinfo = "text",
              text = ~paste("<b>", municipio, "</b><br>Fecha: ", format(month, "%b %Y"),
                            "<br>Intervenciones: ", total)) %>%
        layout(
          xaxis = list(title = "Mes", gridcolor = '#444444'),
          yaxis = list(title = "Municipio", categoryorder = "total ascending",
                       gridcolor = '#444444', tickfont = list(size = 10)),
          plot_bgcolor = '#1e1e1e',
          paper_bgcolor = '#1e1e1e',
          font = list(color = '#FFFFFF')
        )
    } else {
      plotly_empty() %>%
        layout(
          title = list(text = "No hay datos disponibles para el heatmap", font = list(color = '#FFFFFF')),
          xaxis = list(title = "Mes", gridcolor = '#444444'),
          yaxis = list(title = "Municipio", gridcolor = '#444444'),
          plot_bgcolor = '#1e1e1e',
          paper_bgcolor = '#1e1e1e',
          font = list(color = '#FFFFFF')
        )
    }
  })


  # # Selected timeline ----
  # output$selected_timeline <- renderPlotly({
  #   req(input$mainTabs == "heatmap_tab", input$municipio_search)
  #   
  #   # Ensure that input$municipio_search is not NULL and has at least one value selected
  #   if (!is.null(input$municipio_search) && length(input$municipio_search) > 0) {
  #     df <- selected_data() %>%
  #       filter(!is.na(fecha_hecho)) %>%
  #       filter(municipio %in% input$municipio_search) %>%
  #       mutate(month = floor_date(fecha_hecho, "month")) %>%
  #       count(month, municipio, name = "total")
  #     
  #     if (nrow(df) > 0) {
  #       p <- plot_ly()
  #       
  #       # Group data by municipio and add a trace for each
  #       df %>%
  #         group_by(municipio) %>%
  #         do(
  #           p = add_trace(
  #             p,
  #             x = ~month,
  #             y = ~total,
  #             type = 'scatter',
  #             mode = 'lines+markers',
  #             name = first(.$municipio),  # Use municipio name for the legend
  #             line = list(), # You can customize line properties here if needed
  #             marker = list() # You can customize marker properties here if needed
  #           )
  #         )
  #       
  #       p <- layout(
  #         p,
  #         title = list(text = "Serie Temporal por Municipio", font = list(size = 14, color = '#FFFFFF')),
  #         xaxis = list(title = "Fecha", gridcolor = '#444444', color = '#FFFFFF'),
  #         yaxis = list(title = "Intervenciones", gridcolor = '#444444', color = '#FFFFFF'),
  #         plot_bgcolor = '#1e1e1e',
  #         paper_bgcolor = '#1e1e1e',
  #         margin = list(t = 40),
  #         font = list(color = '#FFFFFF'),
  #         showlegend = TRUE  # Make sure the legend is displayed
  #       )
  #       
  #       p # Print the plot
  #     } else {
  #       plotly_empty() %>%
  #         layout(
  #           title = list(text = "No hay datos disponibles para la serie temporal seleccionada", font = list(size = 14, color = '#FFFFFF')),
  #           xaxis = list(title = "Fecha", gridcolor = '#444444', color = '#FFFFFF'),
  #           yaxis = list(title = "Intervenciones", gridcolor = '#444444', color = '#FFFFFF'),
  #           plot_bgcolor = '#1e1e1e',
  #           paper_bgcolor = '#1e1e1e',
  #           margin = list(t = 40),
  #           font = list(color = '#FFFFFF')
  #         )
  #     }
  #   } else {
  #     plotly_empty() %>%
  #       layout(
  #         title = list(text = "Seleccione al menos un municipio", font = list(size = 14, color = '#FFFFFF')),
  #         xaxis = list(title = "Fecha", gridcolor = '#444444', color = '#FFFFFF'),
  #         yaxis = list(title = "Intervenciones", gridcolor = '#444444', color = '#FFFFFF'),
  #         plot_bgcolor = '#1e1e1e',
  #         paper_bgcolor = '#1e1e1e',
  #         margin = list(t = 40),
  #         font = list(color = '#FFFFFF')
  #       )
  #   }
  # })
  # 
  # Reset search ----
  observeEvent(input$reset_search, {
    updateSelectizeInput(session, "municipio_search", selected = character(0))
  })
}

# Shiny app ----
shinyApp(ui = ui, server = server)
