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

# UI ----
ui <- fluidPage(
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
      )
    )
  ),
  
  # Sidebar layout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("dataset", "Acción:", choices = names(working_data)),
      
      # Dataset description ----
      div(
        style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
        htmlOutput("dataset_description")
      ),
      
      # Map Configuration ----
      div(
        class = "time-control",
        h4("Configuración del mapa"),
        sliderInput(
          "heatIntensity",
          "Intensidad:",
          min = 0.1,
          max = 1.0,
          value = 0.6,
          step = 0.1,
          animate = FALSE
        ),
        sliderInput(
          "heatRadius",
          "Radio:",
          min = 5,
          max = 30,
          value = 15,
          step = 5,
          animate = FALSE
        )
      ),
      
      # Loading Status ----
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
    
    # Main panel ----
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "mainTabs",
        
        # Map tab ----
        tabPanel(
          "Mapa de Calor (Cantidades Intervenidas)",
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
            
            # Absolute panel for checkbox ----
            absolutePanel(
              bottom = 10,
              left = 10,
              style = paste0(
                "z-index:500; background-color: rgba(240,240,240,0.8); padding: 8px; border-radius: 5px;",
                "box-shadow: 0 0 15px rgba(0,0,0,0.2); color: black;"
              ),
              checkboxInput("showClusters", tags$span("Número de Intervenciones (Clusters por Municipio)", style = "color: black;"), value = FALSE, width = "100%")
            )
          ),
          
          # Timeline plot ----
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
        
        # Heatmap tab ----
        tabPanel(
          "Distribución Temporal por Municipio",
          value = "heatmap_tab",
          div(
            style = "position:relative;",
            plotlyOutput("heatmapTime", height = "60vh"),
            div(
              id = "loading-heatmap",
              class = "loading-indicator",
              style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
              tags$div(
                class = "loading-spinner",
                "Cargando distribución temporal..."
              )
            )
          ),
          
          # Description ----
          p(
            "Aquí se muestra la concentración de intervenciones a lo largo del tiempo y por municipio."
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Reactive values ----
  dataset_cache <- reactiveVal(list())
  processing_status <- reactiveVal("idle")
  
  # Selected data ----
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
    
    validate(
      need(
        nrow(df) > 0,
        "El conjunto seleccionado no contiene coordenadas válidas"
      )
    )
    
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
      center = list(
        lng = median(df$LONGITUD, na.rm = TRUE),
        lat = median(df$LATITUD, na.rm = TRUE)
      ),
      zoom = ifelse(nrow(df) > 1000, 5, 7)
    )
  })
  
  # Observe processing status ----
  observe({
    status <- processing_status()
    if (status == "loading") {
      shinyjs::show("loading-status")
    } else {
      shinyjs::hide("loading-status")
    }
  })
  
  # Dataset description memo ----
  dataset_desc_memo <- memoise::memoise(function(dataset_name) {
    dataset_descriptions[[dataset_name]]
  })
  
  # Output dataset description ----
  output$dataset_description <- renderUI({
    req(input$dataset)
    desc <- dataset_desc_memo(input$dataset)
    HTML(paste0("<strong>Descripción:</strong> ", desc))
  })
  
  # Output map ----
  output$map <- renderLeaflet({
    map_info <- map_data()
    df <- map_info$data
    
    validate(need(nrow(df) > 0, "No hay datos válidos para mostrar"))
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(
        lng = map_info$center$lng,
        lat = map_info$center$lat,
        zoom = map_info$zoom
      ) %>%
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
      # Add mini map to bottom right corner
      addMiniMap(
        tiles = providers$CartoDB.DarkMatter,
        toggleDisplay = TRUE,
        position = "bottomright",
        width = 150,
        height = 150,
        zoomLevelOffset = -5,
        zoomAnimation = FALSE,
        autoToggleDisplay = TRUE,
        minimized = FALSE,
        aimingRectOptions = list(
          color = "#FF0000",
          weight = 1,
          fillOpacity = 0.3
        ),
        shadowRectOptions = list(
          color = "#0000FF",
          weight = 1,
          fillOpacity = 0,
          fillColor = "#0000FF"
        ),
        strings = list(
          hideText = "Ocultar Mini Mapa",
          showText = "Mostrar Mini Mapa"
        )
      )
  })
  
  # Observe show clusters ----
  observe({
    req(input$showClusters, map_data())
    df <- map_data()$data
    
    map_proxy <- leafletProxy("map")
    map_proxy %>% clearMarkerClusters()
    
    if (input$showClusters) {
      popup_content <- ~paste(
        if ("municipio" %in% colnames(df)) paste0("<b>Municipio:</b> ", municipio, "<br/>"),
        if ("fecha_hecho" %in% colnames(df)) paste0("<b>Fecha:</b> ", format(fecha_hecho, "%d-%m-%Y"), "<br/>"),
        if ("cantidad" %in% colnames(df)) paste0("<b>Cantidad:</b> ", cantidad)
      )
      
      map_proxy %>%
        addCircleMarkers(
          data = df,
          lng = ~ LONGITUD,
          lat = ~ LATITUD,
          radius = 2,
          color = "#FFFFFF",
          fillOpacity = 0.5,
          weight = 1,
          popup = popup_content,
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 30,
            spiderfyOnMaxZoom = TRUE
          )
        )
    }
  })
  
  # Output timeline ----
  output$timeline <- renderPlotly({
    req(input$mainTabs == "map_tab")
    df <- selected_data()
    
    validate(need("fecha_hecho" %in% colnames(df), "Datos temporales no disponibles"))
    
    # Aggregate data by month for the timeline
    df_time <- df %>%
      mutate(month = floor_date(as.Date(fecha_hecho), "month")) %>%
      count(month, name = "cantidad")
    
    # Add missing dates for continuity
    if (nrow(df_time) > 1) {
      date_range <- seq(
        from = min(df_time$month, na.rm = TRUE),
        to = max(df_time$month, na.rm = TRUE),
        by = "month"
      )
      
      all_dates <- data.frame(month = date_range)
      
      df_time <- all_dates %>%
        left_join(df_time, by = "month") %>%
        mutate(cantidad = replace_na(cantidad, 0))
    }
    
    # Create a column for color mapping based on quantity
    df_time <- df_time %>%
      mutate(cantidad_norm = scales::rescale(cantidad, to = c(0, 1)))
    
    # Create Plotly directly instead of ggplot for better control
    p <- plot_ly(
      df_time,
      x = ~month,
      y = ~cantidad,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(
        color = '#440154',
        width = 2
      ),
      marker = list(
        size = 8,
        color = ~viridis::viridis(length(month))[rank(cantidad_norm)],
        line = list(width = 1, color = '#FFFFFF')
      ),
      hoverinfo = 'text',
      text = ~paste(
        '<b>Fecha:</b>', format(month, "%B %Y"),
        '<br><b>Intervenciones:</b>', cantidad
      )
    ) %>%
      layout(
        title = list(
          text = "Evolución Temporal de Intervenciones",
          font = list(size = 16, color = '#FFFFFF')
        ),
        xaxis = list(
          title = "Fecha",
          gridcolor = '#444444',
          zerolinecolor = '#666666',
          tickformat = "%b %Y",
          tickangle = -45,
          hoverformat = "%b %Y",
          tickfont = list(color = '#FFFFFF'),
          titlefont = list(color = '#FFFFFF')
        ),
        yaxis = list(
          title = "Número",
          gridcolor = '#444444',
          zerolinecolor = '#666666',
          tickfont = list(color = '#FFFFFF'),
          titlefont = list(color = '#FFFFFF')
        ),
        plot_bgcolor = '#1e1e1e',
        paper_bgcolor = '#1e1e1e',
        margin = list(l = 60, r = 30, b = 60, t = 40),
        hovermode = 'x unified',
        showlegend = FALSE
      )
    
    return(p)
  })
  
  # Output heatmap time ----
  output$heatmapTime <- renderPlotly({
    req(input$mainTabs == "heatmap_tab")
    df <- selected_data()
    
    geo_column <- first(intersect(c("municipio", "MUNICIPIO", "DEPARTAMENTO"), colnames(df)))
    
    validate(
      need(
        !is.null(geo_column) && "fecha_hecho" %in% colnames(df),
        "Datos requeridos no disponibles"
      )
    )
    
    df_agg <- df %>%
      mutate(
        month = floor_date(as.Date(fecha_hecho), "month"),
        geo_unit = .data[[geo_column]]
      ) %>%
      count(month, geo_unit, name = "total") %>%
      filter(total > 0) %>%
      arrange(month)
    
    if (n_distinct(df_agg$geo_unit) > 30) {
      top_locations <- df_agg %>%
        group_by(geo_unit) %>%
        summarize(total = sum(total)) %>%
        slice_max(total, n = 3000) %>%
        pull(geo_unit)
      
      df_agg <- df_agg %>% filter(geo_unit %in% top_locations)
    }
    
    plot_ly(
      df_agg,
      x = ~month,
      y = ~geo_unit,
      z = ~total,
      type = "heatmap",
      colorscale = "Viridis",
      hovertemplate = paste(
        "<b>%{y}</b><br>Fecha: %{x|%b %Y}<br>Intervenciones: %{z}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Lugar"),
        yaxis = list(categoryorder = "total ascending")
      )
  })
}

# Shiny app ----
shinyApp(ui = ui, server = server)
