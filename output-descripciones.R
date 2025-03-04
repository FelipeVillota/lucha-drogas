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
    "ASPERSION" = "Las zonas de aspersión aérea sobre cultivos ilícitos en Colombia han sido un punto clave en la lucha contra el narcotráfico y, al mismo tiempo, un tema de fuerte controversia. Desde finales del siglo XX, el gobierno colombiano, con apoyo de Estados Unidos a través del Plan Colombia, ha utilizado la fumigación con glifosato para erradicar cultivos de coca, amapola y marihuana en regiones como el Catatumbo, el Putumayo, el Guaviare y el Cauca. Sin embargo, esta estrategia ha generado debates debido a sus impactos ambientales y sociales, pues comunidades campesinas e indígenas han denunciado la afectación de sus fuentes de agua, cultivos legales y salud. En 2015, la Corte Constitucional suspendió la aspersión con glifosato debido a sus posibles efectos cancerígenos, pero en los años recientes ha habido intentos de reactivar su uso en el marco de la política antidrogas del país.",
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

