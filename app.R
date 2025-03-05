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

  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("dataset", "Acción:", choices = names(working_data)),

      div(
        style = "margin-top: 20px; font-style: italic; color: #bbbbbb;",
        htmlOutput("dataset_description")
      ),

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
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "mainTabs",
        tabPanel(
          "Mapa de Calor",
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
              checkboxInput("showClusters", tags$span("Mostrar Clusters (Agregación de eventos)", style = "color: black;"), value = FALSE, width = "100%")
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
        tabPanel(
          "Distribución Temporal",
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
  dataset_cache <- reactiveVal(list())
  processing_status <- reactiveVal("idle")

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

  observe({
    status <- processing_status()
    if (status == "loading") {
      shinyjs::show("loading-status")
    } else {
      shinyjs::hide("loading-status")
    }
  })

  dataset_desc_memo <- memoise::memoise(function(dataset_name) {
    dataset_descriptions[[dataset_name]]
  })

  output$dataset_description <- renderUI({
    req(input$dataset)
    desc <- dataset_desc_memo(input$dataset)
    HTML(paste0("<strong>Descripción:</strong> ", desc))
  })

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
      )
  })

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

  output$timeline <- renderPlotly({
    req(input$mainTabs == "map_tab")
    df <- selected_data()

    validate(need("fecha_hecho" %in% colnames(df), "Datos temporales no disponibles"))

    df_time <- df %>%
      mutate(month = floor_date(as.Date(fecha_hecho), "month")) %>%
      count(month, name = "cantidad")

    p <- ggplot(df_time, aes(x = month, y = cantidad)) +
      geom_line(color = "#00BFC4", size = 1) +
      geom_point(color = "#FFFFFF", size = 2, alpha = 0.8) +
      labs(x = "Fecha", y = "Número de Intervenciones") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.background = element_rect(fill = "#1e1e1e"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white")
      )

    ggplotly(p) %>% layout(hovermode = "x")
  })

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
        yaxis = list(title = "Lugar")) %>%  layout(yaxis = list(categoryorder = "total ascending"))
  })
}

shinyApp(ui = ui, server = server)

