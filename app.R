#-----------------------------VERSION 1.3---------------------------------------

# # COLOMBIA VS DROGAS - DATA VISUALIZATION APP
# #
# # Author: Luis Felipe Villota Macías
# # Description: Shiny application for visualizing drug intervention data in Colombia


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

# Pre-launch sourcing ----

source("precomputation.R")
source("output-descripciones.R")


precomputed <- readRDS("data/preprocessed_data.rds")
processed_data <- precomputed$processed_data
municipality_list <- precomputed$municipality_list
temporal_aggregations <- precomputed$temporal_aggregations
top_municipalities <- precomputed$top_municipalities

# UI ------------------
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
          "Integración y exploración espacio-temporal de todos los datos disponibles del <a href='https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica'
          target='_blank' style='color: #007bff; text-decoration: none; font-weight: bold;'>Ministerio de Defensa</a> relativos a los avances en la lucha contra el problema mundial de las drogas
          (consulta del 16 de enero de 2025)"
        )
      ),
      p("Autor: Luis Felipe Villota Macías", style = "font-size: 14px; color: #888; margin-top: 10px;")
    )
  ),
  
  ## Sidebar layout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("dataset", "Tipo de operación:", choices = names(processed_data)),
      
      ### Dataset description ----
      div(
        style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
        htmlOutput("dataset_description")
      ),
      
      ### Map Configuration ----
      div(
        class = "time-control",
        h4("Configuración del mapa de calor"),
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
          "Panorama nacional",
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
              top = 10,    # Changed from bottom
              right = 10,  # Changed from left
              style = paste0(
                "z-index:500; background-color: rgba(240,240,240,0.8); padding: 8px; border-radius: 5px;",
                "box-shadow: 0 0 15px rgba(0,0,0,0.2); color: black;"
              ),
              checkboxInput("showClusters", tags$span("Clusters municipales", style = "color: black;"), value = FALSE, width = "100%"),
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
            style = "position:relative; height: 90vh;",
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
                     plotlyOutput("selected_timeline", height = "25vh"),
                     plotlyOutput("municipio_barchart", height = "25vh") 
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
  processing_status <- reactiveVal("idle")
  
  ### Selected data ----
  selected_data <- reactive({
    req(input$dataset)
    processed_data[[input$dataset]]
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
    HTML(paste0("<strong>Descripción:</strong> ", dataset_descriptions[[input$dataset]], dataset_legend_info[[input$dataset]]))
  })
  
  # Initialize selectizeInput when dataset changes
  observe({
    req(input$dataset)
    munis <- municipality_list[[input$dataset]]
    updateSelectizeInput(session, "municipio_search", choices = munis, server = TRUE)
  })
  
  # Map output ----
  output$map <- renderLeaflet({
    req(input$mainTabs == "map_tab")
    map_info <- map_data()
    df <- map_info$data
    validate(need(nrow(df) > 0, "No hay datos válidos para mostrar"))
    
    #### Legend palette----
    pal <- colorNumeric(
      palette = rev(c("#440154", "#414487", "#2A788E", "#22A884", "#FDE725")), # Reversed for correct legend order
      domain = df$cantidad
    )
    
    # Create the leaflet map-----
    map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = map_info$center$lng, lat = map_info$center$lat, zoom = map_info$zoom) %>%
      
      # Heatmap Layer ----
    addHeatmap(
      data = df,
      lng = ~ LONGITUD,
      lat = ~ LATITUD,
      intensity = if ("cantidad" %in% colnames(df)) ~ cantidad else 1,
      radius = input$heatRadius * 1,
      blur = input$heatRadius * 1.8,
      max = input$heatIntensity,
      gradient <- c("#440154", "#414487", "#2A788E", "#22A884", "#FDE725")
    ) %>%
      
      # MiniMap ----
    addMiniMap(
      tiles = providers$CartoDB.DarkMatterNoLabels,
      position = "bottomleft",
      width = 120,
      height = 120,
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
    )  %>% 
      
      # Legend and heat scale ----
    
    addLegend(
      position = "bottomright",
      pal = pal,
      values = df$cantidad,
      title = paste0(
        "<div style='font-size: 12px; line-height: 1.3;'>",
        paste0(
          description_unidad_medida[[input$dataset]],
          ifelse(substr(description_unidad_medida[[input$dataset]], nchar(description_unidad_medida[[input$dataset]]), nchar(description_unidad_medida[[input$dataset]])) == "D", "ES", "S")
        ),
        "<br>",
        "</div>"
      ),
      opacity = 1,
      labFormat = labelFormat(
        transform = function(x) {
          breaks <- pretty(x, n = 3)
          breaks <- sort(breaks, decreasing = TRUE)
          return(breaks)
        }
      ),
      bins = 5,
      na.label = "Sin datos"
    )
    
    
    # Cluster observer ----
    
    observe({
      req(input$showClusters, map_data())
      
      # Extract data and leaflet proxy
      df <- map_data()$data
      map_proxy <- leafletProxy("map")
      
      # Clear existing markers and clusters
      map_proxy %>%
        clearMarkers() %>%
        clearMarkerClusters()
      
      # Add cluster markers if the condition is met and data exists
      if (nrow(df) > 0) {
        # Aggregate data by municipality *before* creating popups (efficiency)
        municipality_counts <- df %>%
          group_by(municipio, departamento, LATITUD, LONGITUD) %>%  # Group by location too!
          summarise(
            count = n(), .groups = "drop"  # Count operations per municipality
          ) %>%
          filter(!is.na(LATITUD), !is.na(LONGITUD)) # remove NA coords
        
        # Create popups (only once, after aggregation)
        popup_content <- sprintf(
          "<b>Municipio:</b> %s<br/><b>Departamento:</b> %s<br/><b>Operaciones totales:</b> %s",
          municipality_counts$municipio,
          municipality_counts$departamento,
          municipality_counts$count
        )
        
        # Add circle markers to the map with cluster options
        map_proxy %>% addCircleMarkers(
          data = municipality_counts,
          lng = ~LONGITUD,
          lat = ~LATITUD,
          radius = 5, # Adjust radius as needed
          weight = 1,
          color = "#FFFF00",  # Change marker color
          fillOpacity = 0.6,  # Adjust fill opacity
          popup = popup_content,
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 40, # adjust
            disableClusteringAtZoom = 8 # adjust
          )
        )
      }
    })
    
    
    map
  })
  
  
  
  # Timeline output ----
  
  output$timeline <- renderPlotly({
    req(input$mainTabs == "map_tab")
    req(input$dataset)
    
    timeline_data <- temporal_aggregations[[input$dataset]]$monthly_counts %>%
      left_join(
        temporal_aggregations[[input$dataset]]$cantidad_sums,
        by = "month"
      ) %>%
      replace_na(list(Cantidad = 0, Cumulative_Cantidad = 0))
    
    validate(need(nrow(timeline_data) > 0, "No hay datos temporales disponibles para este dataset."))
    
    plot_ly(data = timeline_data) %>%
      
      # Operaciones (line chart - left axis)
      
      add_lines(
        x = ~month,
        y = ~Operaciones,
        name = "Operaciones",
        line = list(
          color = "rgba(33, 145, 140, 0.9)",
          width = 2.5,
          shape = "spline",
          smoothing = 0.8
        ),
        fill = 'tozeroy',
        fillcolor = "rgba(33, 145, 140, 0.15)", 
        hovertemplate = "<b>%{x|%b %Y}</b><br>Operaciones: %{y}<extra></extra>"
      ) %>%
      
      # Cantidad (bar chart - right axis)
      
      add_bars(
        x = ~month,
        y = ~Cantidad,
        name = "Cantidad intervenida",
        marker = list(
          color = "rgba(255, 215, 0, 0.3)",
          line = list(color = "rgba(255, 215, 0, 0.8)", width = 0.8)
        ),
        hovertemplate = "<b>%{x|%b %Y}</b><br>Cantidad: %{y}<extra></extra>",
        yaxis = "y2"
      ) %>%
      
      # Layout configuration
      
      layout(
        title = list(
          text = "Evolución temporal (nivel nacional, agregación mensual)",
          font = list(color = "#FFFFFF", size = 18, family = "Arial"),
          y = 0.90,
          x = 0.05,
          xanchor = "left"
        ),
        xaxis = list(
          title = list(text = "FECHA", standoff = 15, font = list(size = 14)),
          gridcolor = "#333333",
          tickformat = "%b %Y",
          tickfont = list(color = "#CCCCCC", size = 12)
        ),
        
        # Dual axis
        
        yaxis = list(
          title = list(text = "OPERACIONES", 
                       font = list(size = 13, color = "#21918c")),
          gridcolor = "#333333",
          zerolinecolor = "#444444",
          tickfont = list(color = "#CCCCCC", size = 12),
          showspikes = TRUE,
          spikethickness = 1,
          spikedash = "dot"
        ),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(
            text = paste0(description_unidad_medida[[input$dataset]],"S"),
            font = list(size = 13, color = "#ffd700")
          ),
          gridcolor = "#333333",
          zeroline = FALSE,
          tickfont = list(color = "#CCCCCC", size = 12)
        ),
        plot_bgcolor = "#1e1e1e",
        paper_bgcolor = "#1e1e1e",
        legend = list(
          orientation = "h",
          xanchor = "center",
          yanchor = "bottom",
          x = 0.5,
          y = 1,
          font = list(color = "#FFFFFF", size = 12),
          bgcolor = "rgba(0,0,0,0)"
        ),
        hoverlabel = list(
          bgcolor = "#2d2d2d",
          bordercolor = "#444444",
          font = list(color = "#FFFFFF", size = 12)
        ),
        margin = list(t = 85, b = 60, l = 70, r = 70),
        hovermode = "x unified",
        hoverdistance = 50,
        spikedistance = 300,
        showlegend = TRUE
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
        modeBarButtonsToAdd = list("resetScale2d"),
        modeBarBgColor = "#2d2d2d",
        modeBarColor = "#CCCCCC"
      )
  })
  
  
  
  
  
  # Heatmap data ----
  heatmap_data <- reactive({
    req(input$dataset)
    temporal_aggregations[[input$dataset]]$heatmap_data
  })
  
  # Heatmap output ----
  observe({
    data <- heatmap_data()
    updateSelectizeInput(session, "municipio_search", choices = unique(data$municipio), server = TRUE)
  })
  
  output$heatmapTime <- renderPlotly({
    req(input$mainTabs == "heatmap_tab")
    data <- filtered_heatmap_data()  # Changed to use filtered data
    
    validate(need(nrow(data) > 0, "No hay datos para mostrar en el heatmap."))
    
    plot_ly(data = data,
            x = ~month,
            y = ~municipio,
            z = ~total,
            type = "heatmap",
            colorscale = "Viridis",
            hovertemplate = paste('<b>Municipio</b>: %{y}<br>',
                                  '<b>Mes</b>: %{x|%B %Y}<br>',
                                  '<b>Total</b>: %{z}<extra></extra>')
    ) %>%
      layout(
        title = list(text = "Panorama nacional", x = 0.5, y=5),
        xaxis = list(title = "Mes", gridcolor = '#444444'),
        yaxis = list(title = "Municipio", categoryorder = "total ascending",
                     gridcolor = '#444444', tickfont = list(size = 7)),
        plot_bgcolor = '#1e1e1e',
        paper_bgcolor = '#1e1e1e',
        font = list(color = '#FFFFFF')
      )
  })
  
  # Filtered heatmap data ----
  filtered_heatmap_data <- reactive({
    data <- heatmap_data()
    if (!is.null(input$municipio_search) && length(input$municipio_search) > 0) {
      data <- data %>% filter(municipio %in% input$municipio_search)
    }
    data
  })
  
  # Selected timeline output ----
  output$selected_timeline <- renderPlotly({
    req(input$mainTabs == "heatmap_tab")
    data <- filtered_heatmap_data()
    
    validate(
      need(length(input$municipio_search) > 0,  # Check for selection instead of data rows
           "Seleccione al menos un municipio para ver la línea de tiempo.")
    )
    
    data %>%
      group_by(month) %>%
      summarise(total = sum(total)) %>%
      plot_ly(x = ~month, y = ~total, type = 'scatter', mode = 'lines',
              line = list(color = '#21918c'),
              fill = 'tozeroy', fillcolor = 'rgba(33,145,140,0.2)',
              hovertemplate = paste('<b>Mes</b>: %{x|%B %Y}<br>',
                                    '<b>Total</b>: %{y}<extra></extra>')) %>%
      layout(
        title = list(text = paste0("Acumulado de ", description_unidad_medida[[input$dataset]],"S"), x = 0.1), 
        xaxis = list(title = "", tickformat = "%b %Y"),
        yaxis = list(title = "Total"),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)',
        font = list(color = '#ffffff')
      )
  })
  
  
  # Bar chart data ----
  municipio_barchart_data <- reactive({
    data <- heatmap_data()
    
    # Filter if municipios are selected
    if (!is.null(input$municipio_search) && length(input$municipio_search) > 0) {
      data <- data %>% 
        filter(municipio %in% input$municipio_search)
    } else {
      # Show top 15 municipios if none selected
      data <- data %>%
        group_by(municipio) %>%
        summarise(total = sum(total)) %>%
        slice_max(total, n = 15)
    }
    
    # Aggregate totals
    data %>%
      group_by(municipio) %>%
      summarise(total = sum(total)) %>%
      arrange(desc(total))
  })
  
  # Bar chart output ----
  output$municipio_barchart <- renderPlotly({
    data <- municipio_barchart_data()
    validate(need(nrow(data) > 0, "No hay datos disponibles para el gráfico de barras."))
    
    plot_ly(data,
            x = ~reorder(municipio, -total),
            y = ~total,
            type = "bar",
            marker = list(
              color = ~total,
              colorscale = "Viridis",
              showscale = TRUE
            ),
            hoverinfo = "text",
            text = ~paste0("<b>", municipio, "</b>\nTotal: ", total)
    ) %>%
      layout(
        title = list(
          text = "Total de cantidades por municipio",
          font = list(color = "#FFFFFF", size = 14)
        ),
        xaxis = list(
          title = "",
          categoryorder = "total descending",
          tickangle = -45,
          tickfont = list(size = 10, color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Total",
          gridcolor = "#444444",
          tickfont = list(color = "#FFFFFF")
        ),
        plot_bgcolor = "#1e1e1e",
        paper_bgcolor = "#1e1e1e",
        margin = list(t = 40, b = 100),
        font = list(color = "#FFFFFF")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # Reset search functionality ----
  observeEvent(input$reset_search, {
    updateSelectizeInput(session, "municipio_search", selected = character(0))
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)