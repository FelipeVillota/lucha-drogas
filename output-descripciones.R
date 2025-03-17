library(lubridate)

# Descripción de tipo de operación -----

dataset_descriptions <- sapply(names(working_data), function(dataset) {
  df <- working_data[[dataset]]
  
  # Descriptive texts
  descriptions <- c(
    "ASPERSION" = "Las zonas de aspersión aérea sobre cultivos ilícitos en Colombia han sido un punto clave en la lucha contra el narcotráfico y, al mismo tiempo, un tema de fuerte controversia. Desde finales del siglo XX, el gobierno colombiano, con apoyo de Estados Unidos a través del Plan Colombia, ha utilizado la fumigación con glifosato para erradicar cultivos de coca, amapola y marihuana en regiones como el Catatumbo, el Putumayo, el Guaviare y el Cauca. Sin embargo, esta estrategia ha generado debates debido a sus impactos ambientales y sociales, pues comunidades campesinas e indígenas han denunciado la afectación de sus fuentes de agua, cultivos legales y salud. En 2015, la Corte Constitucional suspendió la aspersión con glifosato debido a sus posibles efectos cancerígenos, pero en los años recientes ha habido intentos de reactivar su uso en el marco de la política antidrogas del país.",
    "DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS" = "Labores de destrucción de laboratorios y centros de producción. Estas actividades buscan desmantelar las infraestructuras utilizadas para la fabricación y procesamiento de sustancias ilícitas como cocaína y heroína. Los operativos incluyen la incautación de precursores químicos y equipos especializados que son esenciales para estas actividades ilegales.",
    "ERRADICACIÓN" = "Registros de erradicación manual y forzosa de cultivos ilícitos. Este proceso implica la eliminación física de cultivos como coca y amapola mediante métodos manuales o mecánicos. Es una estrategia clave en los esfuerzos para reducir la producción de drogas ilícitas en áreas rurales.",
    "INCAUTACIÓN DE BASE DE COCA" = "Confiscaciones en distintas regiones. La base de coca es un producto intermedio en el proceso de fabricación de cocaína. Las incautaciones buscan interrumpir las cadenas de suministro antes de que se complete la producción final.",
    "INCAUTACIÓN DE BASUCO" = "Decomisos en operaciones policiales y militares. El basuco es un subproducto barato y altamente adictivo derivado del procesamiento de cocaína. Su incautación es parte esencial del combate al microtráfico en zonas urbanas.",
    "INCAUTACIÓN DE COCAINA" = "Incautación de cocaína. Estas operaciones se realizan tanto en zonas rurales como urbanas e incluyen decomisos en rutas terrestres, marítimas y aéreas. La cocaína incautada suele ser destruida bajo supervisión judicial.",
    "INCAUTACIÓN DE HEROINA" = "Intervenciones con incautación de heroína. Este opioide altamente adictivo es objeto de decomisos frecuentes en operaciones contra el tráfico internacional y local.",
    "INCAUTACIÓN DE MARIHUANA" = "Operativos con decomisos de marihuana. La marihuana sigue siendo una sustancia ampliamente cultivada e incautada en diferentes regiones del país."
  )
  
  paste0(descriptions[dataset])
}, USE.NAMES = TRUE)

# Descripción en leyenda de mapa -----

dataset_legend_info <- sapply(names(working_data), function(dataset) {
  df <- working_data[[dataset]]
  
  # Date information
  if ("fecha_hecho" %in% colnames(df)) {
    min_date <- min(df$fecha_hecho, na.rm = TRUE)
    max_date <- max(df$fecha_hecho, na.rm = TRUE)
    time_diff <- as.period(interval(min_date, max_date))
    time_range <- sprintf("%d años, %d meses, %d días", 
                          time_diff$year, time_diff$month, time_diff$day)
    date_info <- sprintf("<br><strong>Fechas:</strong> %s a %s <br><strong>Rango:</strong> %s", 
                         min_date, max_date, time_range)
  } else {
    date_info <- "<br><strong>Fechas:</strong> No disponibles"
  }
  
  # Total event count
  if ("fecha_hecho" %in% colnames(df)) {
    total_events <- sum(!is.na(df$fecha_hecho))
    event_info <- sprintf("<br><strong>Eventos totales:</strong> %s", format(total_events, big.mark = ",", scientific = FALSE))
  } else {
    event_info <- "<br><strong>Eventos totales:</strong> No disponible"
  }
  
  # Unique municipality count
  if ("municipio" %in% colnames(df)) {
    unique_mun <- length(unique(na.omit(df$municipio)))
    mun_info <- sprintf("<br><strong>Municipios:</strong> %s", format(unique_mun, big.mark = ",", scientific = FALSE))
  } else {
    mun_info <- "<br><strong>Municipios:</strong> No disponible"
  }
  
  # Measurement unit
  if ("unidad" %in% colnames(df)) {
    unidad <- unique(na.omit(df$unidad))
    unidad <- ifelse(length(unidad) > 1, "múltiples unidades", unidad)
    unidad_info <- sprintf("<br><strong>Unidad de medida:</strong> %s", unidad)
  } else {
    unidad_info <- "<br><strong>Unidad de medida:</strong> No disponible"
  }
  
  # Total quantity
  if ("cantidad" %in% colnames(df)) {
    total_cantidad <- formatC(sum(df$cantidad, na.rm = TRUE), 
                              format = "f", big.mark = ",", digits = 2)
    cantidad_info <- sprintf("<br><strong>Total intervenido:</strong> %s", total_cantidad)
  } else {
    cantidad_info <- "<br><strong>Total:</strong> No disponible"
  }
  
  paste0(date_info, event_info, mun_info, unidad_info, cantidad_info)
}, USE.NAMES = TRUE)


# Descripción unidad de medida -----

description_unidad_medida <- sapply(names(working_data), function(dataset) {
  df <- working_data[[dataset]]
  
  
  # Measurement unit
  if ("unidad" %in% colnames(df)) {
    unidad <- unique(na.omit(df$unidad))
    unidad <- ifelse(length(unidad) > 1, "múltiples unidades", unidad)
    unidad_info <- sprintf(unidad)
  } else {
    unidad_info <- "<br><strong>Unidad de medida:</strong> No disponible"
  }
  
  
  paste0(unidad_info)
}, USE.NAMES = TRUE)
