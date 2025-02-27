library(lubridate)

dataset_descriptions <- sapply(names(working_data), function(dataset) {
  df <- working_data[[dataset]]
  
  # Verificar si existen las columnas necesarias para fechas
  if ("fecha_hecho" %in% colnames(df)) {
    min_date <- min(df$fecha_hecho, na.rm = TRUE)
    max_date <- max(df$fecha_hecho, na.rm = TRUE)
    
    # Calcular rango de tiempo en años, meses y días
    time_diff <- as.period(interval(min_date, max_date))
    time_range <- sprintf("%d años, %d meses, %d días", 
                          time_diff$year, time_diff$month, time_diff$day)
    
    date_info <- sprintf("<br><strong>Fechas:</strong> %s - %s <br><strong>Rango:</strong> %s", 
                         min_date, max_date, time_range)
  } else {
    date_info <- "<br><strong>Fechas:</strong> No disponibles"
  }
  
  # Calcular suma total de 'cantidad' si existe la columna
  if ("cantidad" %in% colnames(df) & "unidad" %in% colnames(df)) {
    total_cantidad <- sum(df$cantidad, na.rm = TRUE)
    unidad <- unique(df$unidad)  # Obtener unidad única
    
    # Formatear con separador de miles
    total_cantidad <- formatC(total_cantidad, format = "f", big.mark = ",", digits = 2)
    
    # Si hay más de una unidad, indicar que son múltiples
    if (length(unidad) > 1) {
      unidad <- "múltiples unidades"
    }
    
    cantidad_info <- sprintf("<br><strong>Total:</strong> %s %s", total_cantidad, unidad)
  } else {
    cantidad_info <- "<br><strong>Total:</strong> No disponible"
  }
  
  descriptions <- c(
    "ASPERSION" = "Zonas de aspersión aérea sobre cultivos ilícitos.",
    "DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS" = "Labores de destrucción de laboratorios y centros de producción.",
    "ERRADICACIÓN" = "Registros de erradicación manual y forzosa de cultivos ilícitos.",
    "INCAUTACIÓN DE BASE DE COCA" = "Confiscaciones en distintas regiones.",
    "INCAUTACIÓN DE BASUCO" = "Decomisos en operaciones policiales y militares.",
    "INCAUTACIÓN DE COCAINA" = "Incautación de cocaína.",
    "INCAUTACIÓN DE HEROINA" = "Intervenciones con incautación de heroína.",
    "INCAUTACIÓN DE MARIHUANA" = "Operativos con decomisos de marihuana."
  )
  
  paste0(descriptions[dataset], date_info, cantidad_info)
}, USE.NAMES = TRUE)

