# library(shiny)
# library(leaflet)
# library(leaflet.extras)
# library(ggplot2)
# library(plotly)
# library(DT)
# library(tidyverse)
# library(lubridate)
# 
# # UI -------
# ui <- fluidPage(
#   tags$head(
#     tags$style(
#       HTML(
#         "body { background-color: #121212; color: #ffffff; }
#         .title-container { text-align: center; padding: 20px; }
#         h2 { font-size: 28px; font-weight: bold; }
#         p { font-size: 16px; max-width: 800px; margin: 0 auto; }
#         .sidebar-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
#         .main-panel { background-color: #1e1e1e; padding: 15px; border-radius: 8px; }
#         .time-control { background-color: #2a2a2a; padding: 10px; border-radius: 5px; margin-top: 10px; }
#         .loading-spinner { 
#           display: inline-block;
#           width: auto;
#           text-align: center;
#           padding: 10px 20px;
#           color: white;
#           background-color: rgba(0,0,0,0.7);
#           border-radius: 5px;
#         }
#         "
#       )
#     ),
#     # Loading indicators JavaScript
#     tags$script(HTML("
#       $(document).on('shiny:busy', function() {
#         $('.loading-indicator').show();
#       });
#       
#       $(document).on('shiny:idle', function() {
#         $('.loading-indicator').hide();
#       });
#       
#       // Optimize Leaflet rendering
#       $(document).on('shiny:connected', function() {
#         Shiny.setInputValue('appReady', true);
#       });
#     "))
#   ),
#   fluidRow(
#     div(
#       class = "title-container",
#       h2("Colombia vs Drogas"),
#       p(
#         HTML(
#           "Integración y exploración espacio-temporal de todos los datos disponibles relativos a los avances en la lucha contra el problema mundial de las drogas del
#           <a href='https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica'
#           target='_blank' style='color: #007bff; text-decoration: none; font-weight: bold;'>Ministerio de Defensa</a>
#           (versión publicada 16 de enero de 2025)"
#         )
#       )
#     )
#   ),
#   
#   # sidebarLayout ----
#   sidebarLayout(
#     sidebarPanel(
#       class = "sidebar-panel",
#       selectInput("dataset", "Campos de acción:", choices = names(working_data)),
#       
#       ## Descripción del dataset ----
#       div(
#         style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
#         htmlOutput("dataset_description")
#       ),
#       
#       ## Map controls----
#       div(
#         class = "time-control",
#         h4("Configuración del mapa"),
#         # Throttled slider inputs for better performance
#         sliderInput(
#           "heatIntensity",
#           "Intensidad:",
#           min = 0.1,
#           max = 1.0,
#           value = 0.6,
#           step = 0.1,
#           animate = FALSE
#         ),
#         sliderInput(
#           "heatRadius",
#           "Radio:",
#           min = 5,
#           max = 30,
#           value = 15,
#           step = 5,
#           animate = FALSE
#         )
#       ),
#       
#       # Data Sampling Option for large datasets
#       div(
#         class = "time-control",
#         h4("Optimización"),
#         checkboxInput("sampleData", "Muestrear datos grandes (más rápido)", value = TRUE),
#         sliderInput(
#           "sampleSize",
#           "Tamaño de muestra máxima:",
#           min = 1000,
#           max = 20000,
#           value = 10000,
#           step = 1000,
#           animate = FALSE
#         )
#       ),
#       # Loading status indicator
#       div(
#         id = "loading-status",
#         class = "loading-indicator",
#         style = "display: none; margin-top: 15px; text-align: center;",
#         tags$div(
#           class = "loading-spinner",
#           "Cargando datos..."
#         )
#       )
#     ),
#     # mainPanel ----
#     mainPanel(
#       class = "main-panel",
#       tabsetPanel(
#         id = "mainTabs",
#         tabPanel(
#           "Mapa",
#           value = "map_tab",
#           
#           # Map with loading indicator
#           div(
#             style = "position:relative;", 
#             leafletOutput("map", height = "60vh"),
#             div(
#               id = "loading-map",
#               class = "loading-indicator",
#               style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
#               tags$div(
#                 class = "loading-spinner",
#                 "Cargando mapa..."
#               )
#             ),
#             absolutePanel(
#               bottom = 10,  
#               left = 10,    
#               style = paste0(
#                 "z-index:500; background-color: rgba(240,240,240,0.8); padding: 8px; border-radius: 5px;",
#                 "box-shadow: 0 0 15px rgba(0,0,0,0.2); color: black;" 
#               ),
#               checkboxInput("showClusters", tags$span("Mostrar Clusters", style = "color: black;"), value = FALSE, width = "100%")
#             )
#           ),
#           
#           # Timeline with loading indicator
#           div(
#             style = "position:relative; margin-top: 10px;",
#             plotlyOutput("timeline", height = "30vh"),
#             div(
#               id = "loading-timeline",
#               class = "loading-indicator",
#               style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
#               tags$div(
#                 class = "loading-spinner",
#                 "Cargando línea de tiempo..."
#               )
#             )
#           )
#         ),
#         tabPanel(
#           "Distribución Temporal",
#           value = "heatmap_tab",
#           
#           # Heatmap with loading indicator
#           div(
#             style = "position:relative;",
#             plotlyOutput("heatmapTime", height = "60vh"),
#             div(
#               id = "loading-heatmap",
#               class = "loading-indicator",
#               style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
#               tags$div(
#                 class = "loading-spinner",
#                 "Cargando mapa de calor..."
#               )
#             )
#           ),
#           p(
#             "El mapa de calor muestra la concentración de intervenciones a lo largo del tiempo y por municipio."
#           )
#         )
#       )
#     )
#   )
# )
# 
# # Server----
# server <- function(input, output, session) {
#   # Initialize reactive cache for datasets
#   dataset_cache <- reactiveVal(list())
#   
#   # Initialize reactive values for current processing status
#   processing_status <- reactiveVal("idle")
#   
#   # Main reactive for data selection with optimizations
#   selected_data <- reactive({
#     req(input$dataset)
#     
#     # Update processing status
#     processing_status("loading")
#     
#     # Check if dataset is already cached
#     cached_data <- dataset_cache()[[input$dataset]]
#     if (!is.null(cached_data)) {
#       # Use cached data
#       df <- cached_data
#       processing_status("idle")
#       return(df)
#     }
#     
#     # If not in cache, load data
#     df <- working_data[[input$dataset]]
#     
#     validate(
#       need(
#         all(c("LATITUD", "LONGITUD") %in% colnames(df)),
#         "El conjunto seleccionado no contiene coordenadas válidas"
#       )
#     )
#     
#     # Ensure date column is properly formatted
#     if ("fecha_hecho" %in% colnames(df)) {
#       df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
#     }
#     
#     # Cache the data
#     current_cache <- dataset_cache()
#     current_cache[[input$dataset]] <- df
#     dataset_cache(current_cache)
#     
#     # Update processing status
#     processing_status("idle")
#     
#     return(df)
#   })
#   
#   # Sample data if needed for performance
#   sampled_data <- reactive({
#     df <- selected_data()
#     
#     # Apply sampling for large datasets if enabled
#     if (input$sampleData && nrow(df) > input$sampleSize) {
#       set.seed(123) # For reproducibility
#       df <- df %>% sample_n(input$sampleSize)
#     }
#     
#     return(df)
#   })
#   
#   # Efficient data extraction for map with debouncing
#   map_data <- reactive({
#     df <- sampled_data()
#     
#     list(
#       data = df,
#       center = if (nrow(df) > 0 && all(!is.na(c(df$LATITUD, df$LONGITUD)))) {
#         list(
#           lng = median(df$LONGITUD, na.rm = TRUE),
#           lat = median(df$LATITUD, na.rm = TRUE)
#         )
#       } else {
#         list(lng = -74.297333, lat = 4.570868)
#       },
#       zoom = if (nrow(df) > 0) ifelse(nrow(df) > 100, 5, 7) else 6
#     )
#   }) %>% debounce(300) # Debounce to prevent rapid re-rendering
#   
#   # Update processing status indicator
#   observe({
#     status <- processing_status()
#     if (status == "loading") {
#       shinyjs::show("loading-status")
#     } else {
#       shinyjs::hide("loading-status")
#     }
#   })
#   
#   # Dataset description with memoization
#   dataset_desc_memo <- memoise::memoise(function(dataset_name) {
#     dataset_descriptions[[dataset_name]]
#   })
#   
#   # output$dataset_description ----
#   output$dataset_description <- renderUI({
#     req(input$dataset)
#     desc <- dataset_desc_memo(input$dataset)
#     HTML(paste0("<strong>Descripción:</strong> ", desc))
#   })
#   
#   # output$map with optimizations ----
#   output$map <- renderLeaflet({
#     map_info <- map_data()
#     df <- map_info$data
#     
#     validate(
#       need(
#         all(c("LATITUD", "LONGITUD") %in% colnames(df)) &&
#           is.numeric(df$LATITUD) && is.numeric(df$LONGITUD),
#         "El conjunto seleccionado no contiene coordenadas válidas"
#       )
#     )
#     
#     # Create base map
#     base_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
#       addProviderTiles(providers$CartoDB.DarkMatter) %>%
#       setView(
#         lng = map_info$center$lng,
#         lat = map_info$center$lat,
#         zoom = map_info$zoom
#       )
#     
#     # Make sure we have data to display
#     if (nrow(df) > 0) {
#       # Add heatmap layer with optimized error handling
#       tryCatch({
#         base_map <- base_map %>%
#           addHeatmap(
#             data = df,
#             lng = ~ LONGITUD,
#             lat = ~ LATITUD,
#             intensity = if ("cantidad" %in% colnames(df))
#               ~ cantidad
#             else
#               1,
#             radius = input$heatRadius,
#             blur = input$heatRadius * 1.5,
#             max = input$heatIntensity,
#             gradient = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000")
#           )
#       }, error = function(e) {
#         # Fallback method using circles
#         base_map <<- base_map %>%
#           addCircles(
#             data = df,
#             lng = ~ LONGITUD,
#             lat = ~ LATITUD,
#             radius = 3000, # 3km radius
#             color = "#FF5733",
#             fillOpacity = 0.7,
#             weight = 1
#           )
#       })
#     }
#     
#     base_map
#   })
#   
#   # Update clusters efficiently with leafletProxy
#   observe({
#     req(input$showClusters, map_data())
#     
#     df <- map_data()$data
#     map_proxy <- leafletProxy("map")
#     
#     if (input$showClusters) {
#       # Create simplified popup content for better performance
#       popup_content <- ~paste(
#         ifelse("municipio" %in% colnames(df), paste0("<b>Municipio:</b> ", municipio, "<br/>"), 
#                ifelse("MUNICIPIO" %in% colnames(df), paste0("<b>Municipio:</b> ", MUNICIPIO, "<br/>"), "")),
#         ifelse("fecha_hecho" %in% colnames(df), paste0("<b>Fecha:</b> ", format(fecha_hecho, "%d-%m-%Y"), "<br/>"), ""),
#         ifelse("cantidad" %in% colnames(df), paste0("<b>Cantidad:</b> ", cantidad), "")
#       )
#       
#       map_proxy %>%
#         clearMarkerClusters() %>%
#         addCircleMarkers(
#           data = df,
#           lng = ~ LONGITUD,
#           lat = ~ LATITUD,
#           radius = 2,
#           color = "#FFFFFF",
#           fillOpacity = 0.5,
#           weight = 1,
#           popup = popup_content,
#           clusterOptions = markerClusterOptions(
#             maxClusterRadius = 30,
#             spiderfyOnMaxZoom = TRUE
#           )
#         )
#     } else {
#       map_proxy %>% clearMarkerClusters()
#     }
#   })
#   
#   # output$timeline with optimizations ----
#   output$timeline <- renderPlotly({
#     req(input$mainTabs == "map_tab")  # Only render when tab is active
#     df <- sampled_data()
#     
#     validate(
#       need(
#         all(c("fecha_hecho") %in% colnames(df)),
#         "El conjunto seleccionado no contiene datos válidos para la línea de tiempo"
#       )
#     )
#     
#     # Optimize data aggregation
#     df_time <- if ("cantidad" %in% colnames(df)) {
#       df %>%
#         mutate(fecha_hecho = as.Date(fecha_hecho)) %>%
#         # Monthly aggregation for better performance with large datasets
#         mutate(month = floor_date(fecha_hecho, "month")) %>%
#         group_by(month) %>%
#         summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
#         rename(fecha_hecho = month)
#     } else {
#       df %>%
#         mutate(fecha_hecho = as.Date(fecha_hecho)) %>%
#         mutate(month = floor_date(fecha_hecho, "month")) %>%
#         group_by(month) %>%
#         summarize(cantidad = n(), .groups = "drop") %>%
#         rename(fecha_hecho = month)
#     }
#     
#     y_label <- if ("cantidad" %in% colnames(df)) "Cantidad" else "Número de Intervenciones"
#     
#     p <- ggplot(df_time, aes(x = fecha_hecho, y = cantidad)) +
#       geom_line(color = "#00BFC4", size = 1) +
#       geom_point(color = "#FFFFFF", size = 2, alpha = 0.8) +
#       labs(
#         title = "Intervenciones a lo Largo del Tiempo",
#         x = "Fecha",
#         y = y_label
#       ) +
#       theme_minimal() +
#       theme(
#         plot.background = element_rect(fill = "#1e1e1e", color = NA),
#         panel.background = element_rect(fill = "#1e1e1e", color = NA),
#         text = element_text(color = "#ffffff"),
#         axis.text = element_text(color = "#ffffff"),
#         axis.title = element_text(color = "#ffffff")
#       )
#     
#     ggplotly(p) %>%
#       layout(hovermode = "x")
#   })
#   
#   # output$heatmapTime with lazy loading ----
#   output$heatmapTime <- renderPlotly({
#     req(input$mainTabs == "heatmap_tab")  # Only render when tab is active
#     df <- sampled_data()
#     
#     # Find geographic column with prioritization
#     geo_column <-
#       if ("municipio" %in% colnames(df)) {
#         "municipio"
#       } else if ("MUNICIPIO" %in% colnames(df)) {
#         "MUNICIPIO"
#       } else if ("DEPARTAMENTO" %in% colnames(df)) {
#         "DEPARTAMENTO"
#       } else {
#         NA
#       }
#     
#     validate(
#       need(
#         all(c("fecha_hecho") %in% colnames(df)) &&
#           !is.na(geo_column),
#         "El conjunto seleccionado no contiene datos válidos para el mapa de calor temporal"
#       )
#     )
#     
#     # Optimize aggregation by limiting to top locations
#     df_agg <- df %>%
#       mutate(
#         month = floor_date(as.Date(fecha_hecho), "month"),
#         geo_unit = df[[geo_column]]
#       ) %>%
#       group_by(month, geo_unit) %>%
#       summarize(
#         count = n(),
#         total_cantidad = if ("cantidad" %in% colnames(df))
#           sum(cantidad, na.rm = TRUE)
#         else
#           n(),
#         .groups = "drop"
#       ) %>%
#       mutate(month_label = format(month, "%b %Y"))
#     
#     # Limit to top 30 locations if there are too many for performance
#     if (length(unique(df_agg$geo_unit)) > 30) {
#       top_locations <- df_agg %>%
#         group_by(geo_unit) %>%
#         summarize(total = sum(if ("cantidad" %in% colnames(df)) total_cantidad else count), .groups = "drop") %>%
#         arrange(desc(total)) %>%
#         head(30) %>%
#         pull(geo_unit)
#       
#       df_agg <- df_agg %>% filter(geo_unit %in% top_locations)
#     }
#     
#     # Create heatmap with optimizations
#     z_value <- if ("cantidad" %in% colnames(df)) "total_cantidad" else "count"
#     z_label <- if ("cantidad" %in% colnames(df)) "Total" else "Número de Intervenciones"
#     
#     plot_ly(
#       df_agg,
#       x = ~ month,
#       y = ~ geo_unit,
#       z = ~ get(z_value),
#       type = "heatmap",
#       colorscale = "Viridis",
#       hovertemplate = paste(
#         "<b>%{y}</b><br>",
#         "Fecha: %{x|%b %Y}<br>",
#         paste0(z_label, ": %{z}<br>"),
#         "<extra></extra>"
#       )
#     ) %>%
#       layout(
#         title = paste0(
#           "Distribución de Intervenciones por ",
#           ifelse(
#             geo_column == "DEPARTAMENTO",
#             "Departamento",
#             "Municipio"
#           ),
#           " y Tiempo"
#         ),
#         xaxis = list(
#           title = "Mes",
#           gridcolor = "#555555",
#           tickformat = "%b %Y"
#         ),
#         yaxis = list(
#           title = ifelse(
#             geo_column == "DEPARTAMENTO",
#             "Departamento",
#             "Municipio"
#           ),
#           gridcolor = "#555555"
#         ),
#         paper_bgcolor = "#1e1e1e",
#         plot_bgcolor = "#1e1e1e",
#         font = list(color = "#ffffff")
#       )
#   })
#   
#   # Add shinyjs to show/hide loading indicators
#   shinyjs::useShinyjs()
# }
# 
# shinyApp(ui, server)


library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)

# UI -------
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
        "
      )
    ),
    # Loading indicators JavaScript
    tags$script(HTML("
      $(document).on('shiny:busy', function() {
        $('.loading-indicator').show();
      });
      
      $(document).on('shiny:idle', function() {
        $('.loading-indicator').hide();
      });
      
      // Optimize Leaflet rendering
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
  
  # sidebarLayout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("dataset", "Campos de acción:", choices = names(working_data)),
      
      ## Descripción del dataset ----
      div(
        style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
        htmlOutput("dataset_description")
      ),
      
      ## Map controls----
      div(
        class = "time-control",
        h4("Configuración del mapa"),
        # Throttled slider inputs for better performance
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
      
      # Data Sampling Option for large datasets
      div(
        class = "time-control",
        h4("Optimización"),
        checkboxInput("sampleData", "Muestrear datos grandes (más rápido)", value = TRUE),
        sliderInput(
          "sampleSize",
          "Tamaño de muestra máxima:",
          min = 1000,
          max = 20000,
          value = 10000,
          step = 1000,
          animate = FALSE
        )
      ),
      # Loading status indicator
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
    # mainPanel ----
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "mainTabs",
        tabPanel(
          "Mapa",
          value = "map_tab",
          
          # Map with loading indicator
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
              checkboxInput("showClusters", tags$span("Mostrar Clusters", style = "color: black;"), value = FALSE, width = "100%")
            )
          ),
          
          # Timeline with loading indicator
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
        tabPanel(
          "Distribución Temporal",
          value = "heatmap_tab",
          
          # Heatmap with loading indicator
          div(
            style = "position:relative;",
            plotlyOutput("heatmapTime", height = "60vh"),
            div(
              id = "loading-heatmap",
              class = "loading-indicator",
              style = "display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%);",
              tags$div(
                class = "loading-spinner",
                "Cargando mapa de calor..."
              )
            )
          ),
          p(
            "El mapa de calor muestra la concentración de intervenciones a lo largo del tiempo y por municipio."
          )
        )
      )
    )
  )
)

# Server----
server <- function(input, output, session) {
  # Initialize reactive cache for datasets
  dataset_cache <- reactiveVal(list())
  
  # Initialize reactive values for current processing status
  processing_status <- reactiveVal("idle")
  
  # Main reactive for data selection with optimizations
  selected_data <- reactive({
    req(input$dataset)
    
    # Update processing status
    processing_status("loading")
    
    # Debugging: Print the selected dataset name
    print(paste("Selected dataset:", input$dataset))
    
    # Check if dataset is already cached
    cached_data <- dataset_cache()[[input$dataset]]
    if (!is.null(cached_data)) {
      # Use cached data
      df <- cached_data
      processing_status("idle")
      return(df)
    }
    
    # If not in cache, load data
    df <- working_data[[input$dataset]]
    
    # Debugging: Print column names of loaded data
    print(paste("Column names in", input$dataset, ":", paste(colnames(df), collapse = ", ")))
    
    validate(
      need(
        all(c("LATITUD", "LONGITUD") %in% colnames(df)),
        "El conjunto seleccionado no contiene coordenadas válidas"
      )
    )
    
    # Ensure date column is properly formatted
    if ("fecha_hecho" %in% colnames(df)) {
      tryCatch({
        df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
      }, error = function(e) {
        print(paste("Error converting fecha_hecho to Date:", e$message))
        # Handle the error gracefully, maybe set fecha_hecho to NA or a default date
        df$fecha_hecho <- as.Date(NA)
      })
    }
    
    # Cache the data
    current_cache <- dataset_cache()
    current_cache[[input$dataset]] <- df
    dataset_cache(current_cache)
    
    # Update processing status
    processing_status("idle")
    
    return(df)
  })
  
  # Sample data if needed for performance
  sampled_data <- reactive({
    df <- selected_data()
    
    # Apply sampling for large datasets if enabled
    if (input$sampleData && nrow(df) > input$sampleSize) {
      set.seed(123) # For reproducibility
      df <- df %>% sample_n(input$sampleSize)
    }
    
    return(df)
  })
  
  # Efficient data extraction for map with debouncing
  map_data <- reactive({
    df <- sampled_data()
    
    list(
      data = df,
      center = if (nrow(df) > 0 && all(!is.na(c(df$LATITUD, df$LONGITUD)))) {
        list(
          lng = median(df$LONGITUD, na.rm = TRUE),
          lat = median(df$LATITUD, na.rm = TRUE)
        )
      } else {
        list(lng = -74.297333, lat = 4.570868)
      },
      zoom = if (nrow(df) > 0) ifelse(nrow(df) > 100, 5, 7) else 6
    )
  }) #%>% debounce(300) # Debounce removed temporarily for debugging
  
  # Update processing status indicator
  observe({
    status <- processing_status()
    if (status == "loading") {
      shinyjs::show("loading-status")
    } else {
      shinyjs::hide("loading-status")
    }
  })
  
  # Dataset description with memoization
  dataset_desc_memo <- memoise::memoise(function(dataset_name) {
    dataset_descriptions[[dataset_name]]
  })
  
  # output$dataset_description ----
  output$dataset_description <- renderUI({
    req(input$dataset)
    desc <- dataset_desc_memo(input$dataset)
    HTML(paste0("<strong>Descripción:</strong> ", desc))
  })
  
  # output$map with optimizations ----
  output$map <- renderLeaflet({
    map_info <- map_data()
    df <- map_info$data
    
    # Debugging: Inspect the structure of df *before* rendering
    str(df)
    print(head(df))
    
    validate(
      need(
        all(c("LATITUD", "LONGITUD") %in% colnames(df)) &&
          is.numeric(df$LATITUD) && is.numeric(df$LONGITUD) &&
          all(!is.na(df$LATITUD)) && all(!is.na(df$LONGITUD)),  # Added NA check
        "El conjunto seleccionado no contiene coordenadas válidas"
      )
    )
    
    # Create base map
    base_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(
        lng = map_info$center$lng,
        lat = map_info$center$lat,
        zoom = map_info$zoom
      )
    
    # Make sure we have data to display
    if (nrow(df) > 0) {
      # Add heatmap layer with optimized error handling
      tryCatch({
        base_map <- base_map %>%
          addHeatmap(
            data = df,
            lng = ~ LONGITUD,
            lat = ~ LATITUD,
            intensity = if ("cantidad" %in% colnames(df))
              ~ cantidad
            else
              1,
            radius = input$heatRadius,
            blur = input$heatRadius * 1.5,
            max = input$heatIntensity,
            gradient = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000")
          )
      }, error = function(e) {
        print(paste("Heatmap error:", e$message))
        # Fallback method using circles
        base_map <<- base_map %>%
          addCircles(
            data = df,
            lng = ~ LONGITUD,
            lat = ~ LATITUD,
            radius = 3000, # 3km radius
            color = "#FF5733",
            fillOpacity = 0.7,
            weight = 1
          )
      })
    }
    
    base_map
  })
  
  # Update clusters efficiently with leafletProxy
  observe({
    req(input$showClusters, map_data())
    
    tryCatch({
      df <- map_data()$data
      map_proxy <- leafletProxy("map")
      
      if (input$showClusters) {
        # Create simplified popup content for better performance
        popup_content <- ~paste(
          ifelse("municipio" %in% colnames(df), paste0("<b>Municipio:</b> ", municipio, "<br/>"),
                 ifelse("MUNICIPIO" %in% colnames(df), paste0("<b>Municipio:</b> ", MUNICIPIO, "<br/>"), "")),
          ifelse("fecha_hecho" %in% colnames(df), paste0("<b>Fecha:</b> ", format(fecha_hecho, "%d-%m-%Y"), "<br/>"), ""),
          ifelse("cantidad" %in% colnames(df), paste0("<b>Cantidad:</b> ", cantidad), "")
        )
        
        map_proxy %>%
          clearMarkerClusters() %>%
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
      } else {
        map_proxy %>% clearMarkerClusters()
      }
    }, error = function(e) {
      print(paste("Error in cluster rendering:", e$message))
    })
  })
  
  # output$timeline with optimizations ----
  output$timeline <- renderPlotly({
    req(input$mainTabs == "map_tab")  # Only render when tab is active
    df <- sampled_data()
    
    validate(
      need(
        all(c("fecha_hecho") %in% colnames(df)),
        "El conjunto seleccionado no contiene datos válidos para la línea de tiempo"
      )
    )
    
    # Optimize data aggregation
    df_time <- if ("cantidad" %in% colnames(df)) {
      df %>%
        mutate(fecha_hecho = as.Date(fecha_hecho)) %>%
        # Monthly aggregation for better performance with large datasets
        mutate(month = floor_date(fecha_hecho, "month")) %>%
        group_by(month) %>%
        summarize(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
        rename(fecha_hecho = month)
    } else {
      df %>%
        mutate(fecha_hecho = as.Date(fecha_hecho)) %>%
        mutate(month = floor_date(fecha_hecho, "month")) %>%
        group_by(month) %>%
        summarize(cantidad = n(), .groups = "drop") %>%
        rename(fecha_hecho = month)
    }
    
    y_label <- if ("cantidad" %in% colnames(df)) "Cantidad" else "Número de Intervenciones"
    
    p <- ggplot(df_time, aes(x = fecha_hecho, y = cantidad)) +
      geom_line(color = "#00BFC4", size = 1) +
      geom_point(color = "#FFFFFF", size = 2, alpha = 0.8) +
      labs(
        title = "Intervenciones a lo Largo del Tiempo",
        x = "Fecha",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.background = element_rect(fill = "#1e1e1e", color = NA),
        text = element_text(color = "#ffffff"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff")
      )
    
    ggplotly(p) %>%
      layout(hovermode = "x")
  })
  
  # output$heatmapTime with lazy loading ----
  output$heatmapTime <- renderPlotly({
    req(input$mainTabs == "heatmap_tab")  # Only render when tab is active
    df <- sampled_data()
    
    # Debugging: Inspect the structure of df before heatmap processing
    print("Data frame for heatmap:")
    str(df)
    print(head(df))
    
    # Find geographic column with prioritization
    geo_column <-
      if ("municipio" %in% colnames(df)) {
        "municipio"
      } else if ("MUNICIPIO" %in% colnames(df)) {
        "MUNICIPIO"
      } else if ("DEPARTAMENTO" %in% colnames(df)) {
        "DEPARTAMENTO"
      } else {
        NA
      }
    
    # Debugging: Print the identified geo_column
    print(paste("Geo column:", geo_column))
    
    validate(
      need(
        all(c("fecha_hecho") %in% colnames(df)) &&
          !is.na(geo_column) && geo_column %in% colnames(df),  # Added check if geo_column exists
        "El conjunto seleccionado no contiene datos válidos para el mapa de calor temporal"
      )
    )
    
    # Further validation: Check for NA values in the geographic column
    if (!is.na(geo_column)) {
      validate(
        need(
          !any(is.na(df[[geo_column]])),
          "La columna geográfica contiene valores faltantes (NA)."
        )
      )
    }
    
    # Optimize aggregation by limiting to top locations
    tryCatch({
      df_agg <- df %>%
        mutate(
          month = floor_date(as.Date(fecha_hecho), "month"),
          geo_unit = df[[geo_column]]
        ) %>%
        group_by(month, geo_unit) %>%
        summarize(
          count = n(),
          total_cantidad = if ("cantidad" %in% colnames(df))
            sum(cantidad, na.rm = TRUE)
          else
            n(),
          .groups = "drop"
        ) %>%
        mutate(month_label = format(month, "%b %Y"))
      
      # Limit to top 30 locations if there are too many for performance
      if (length(unique(df_agg$geo_unit)) > 30) {
        top_locations <- df_agg %>%
          group_by(geo_unit) %>%
          summarize(total = sum(if ("cantidad" %in% colnames(df)) total_cantidad else count), .groups = "drop") %>%
          arrange(desc(total)) %>%
          head(30) %>%
          pull(geo_unit)
        
        df_agg <- df_agg %>% filter(geo_unit %in% top_locations)
      }
      
      # Create heatmap with optimizations
      z_value <- if ("cantidad" %in% colnames(df)) "total_cantidad" else "count"
      z_label <- if ("cantidad" %in% colnames(df)) "Total" else "Número de Intervenciones"
      
      plot_ly(
        df_agg,
        x = ~ month,
        y = ~ geo_unit,
        z = ~ get(z_value),
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Fecha: %{x|%b %Y}<br>",
          paste0(z_label, ": %{z}<br>"),
          "<extra></extra>"
        )
      )
    }, error = function(e) {
      print(paste("Error generating heatmap:", e$message))
      # Return an empty plot as a fallback
      plotly() %>% layout(annotations = list(text = "No se pudo generar el mapa de calor.", showarrow = FALSE))
    })
  })
}


shinyApp(ui = ui, server = server)
