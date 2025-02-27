create_timeline_plot <- function(df) {
  df <- df %>% 
    mutate(fecha_hecho = as.Date(fecha_hecho))
  
  p <- ggplot(df, aes(x = fecha_hecho, y = cantidad,
                      text = paste("Fecha:", fecha_hecho,
                                   "<br>Municipio:", municipio,
                                   "<br>Cantidad:", cantidad))) +
    geom_point(size = 1, color = "#00BFC4", alpha = 0.5) +
    labs(x = "Fecha", y = "Cantidad", 
         title = "Cantidad reportada a lo largo del tiempo") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1e1e1e"),
      panel.background = element_rect(fill = "#1e1e1e"),
      text = element_text(color = "#ffffff"),
      axis.text = element_text(color = "#ffffff"),
      axis.title = element_text(color = "#ffffff")
    )
  
  ggplotly(p, tooltip = "text")
}

