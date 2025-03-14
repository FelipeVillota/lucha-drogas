# # Libraries ----
# library(tidyverse)
# library(lubridate)
# 
# # Load working data ----
# precomputed_data <- readRDS("C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/working_data.rds")
# 
# # Create preprocessed data structures ----
# processed_data <- list()
# municipality_list <- list()
# temporal_aggregations <- list()
# top_municipalities <- list()
# 
# ## Main processing loop ----
# for (dataset_name in names(precomputed_data)) {
#   cat("Processing", dataset_name, "...\n")
#   
#   ### Get dataset and filter invalid coordinates ----
#   df <- precomputed_data[[dataset_name]] %>%
#     filter(
#       !is.na(LATITUD),
#       !is.na(LONGITUD),
#       between(LATITUD, -4.23, 13.5),
#       between(LONGITUD, -82.0, -66.87)
#     )
#   
#   ## Convert date if it exists ----
#   if ("fecha_hecho" %in% colnames(df)) {
#     tryCatch({
#       df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
#     }, error = function(e) {
#       df$fecha_hecho <- as.Date(NA)
#     })
#   }
#   
#   ## Store cleaned dataset ----
#   processed_data[[dataset_name]] <- df
#   
#   ## Create municipality list for this dataset -----
#   if ("municipio" %in% colnames(df)) {
#     municipality_list[[dataset_name]] <- unique(df$municipio)
#   } else {
#     municipality_list[[dataset_name]] <- character(0)
#   }
#   
#   ## Create temporal aggregations if date exists ----
#   if ("fecha_hecho" %in% colnames(df) && any(!is.na(df$fecha_hecho))) {
#     # Monthly time series for overall timeline using cantidad
#     monthly_counts <- df %>%
#       filter(!is.na(fecha_hecho)) %>%
#       mutate(month = floor_date(fecha_hecho, "month")) %>%
#       group_by(month) %>%
#       summarise(Acumulado = sum(cantidad, na.rm = TRUE)) %>%
#       ungroup()
#     
#     ## Complete the time series with zeros for missing months -----
#     if (nrow(monthly_counts) > 0) {
#       date_range <- seq(min(monthly_counts$month), max(monthly_counts$month), by = "month")
#       monthly_counts <- data.frame(month = date_range) %>%
#         left_join(monthly_counts, by = "month") %>%
#         mutate(Acumulado = replace_na(Acumulado, 0))
#     }
#     
#     ## Municipality-month heatmap data using cantidad ----
#     heatmap_data <- df %>%
#       filter(!is.na(fecha_hecho)) %>%
#       mutate(
#         month = floor_date(fecha_hecho, "month"),
#         municipio = as.character(municipio)
#       ) %>%
#       group_by(month, municipio) %>%
#       summarise(total = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
#       filter(total > 0) %>%
#       arrange(month)
#     
#     ## Calculate top municipalities by cantidad ----
#     if (nrow(heatmap_data) > 0) {
#       top_munis <- heatmap_data %>%
#         group_by(municipio) %>%
#         summarise(total = sum(total)) %>%
#         slice_max(total, n = 100) %>%
#         pull(municipio)
#       
#       top_municipalities[[dataset_name]] <- top_munis
#     } else {
#       top_municipalities[[dataset_name]] <- character(0)
#     }
#     
#     ## Store temporal aggregations ----
#     temporal_aggregations[[dataset_name]] <- list(
#       monthly_counts = monthly_counts,
#       heatmap_data = heatmap_data
#     )
#   } else {
#     temporal_aggregations[[dataset_name]] <- list(
#       monthly_counts = data.frame(),
#       heatmap_data = data.frame()
#     )
#     top_municipalities[[dataset_name]] <- character(0)
#   }
#   
#   cat("Completed processing", dataset_name, "\n")
# }
# 
# ## Create metadata for caching ----
# metadata <- list(
#   creation_date = Sys.time(),
#   datasets = names(precomputed_data),
#   record_counts = sapply(processed_data, nrow)
# )
# 
# # Save all preprocessed data -----
# preprocessed_data <- list(
#   processed_data = processed_data,
#   municipality_list = municipality_list,
#   temporal_aggregations = temporal_aggregations,
#   top_municipalities = top_municipalities,
#   metadata = metadata
# )
# 
# saveRDS(preprocessed_data, "data/preprocessed_data.rds")
# cat("Preprocessing completed and data saved.\n")

# Libraries ----
library(tidyverse)
library(lubridate)

# Load working data ----
precomputed_data <- readRDS("C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/working_data.rds")

# Create preprocessed data structures ----
processed_data <- list()
municipality_list <- list()
temporal_aggregations <- list()
top_municipalities <- list()

## Main processing loop ----
for (dataset_name in names(precomputed_data)) {
  cat("Processing", dataset_name, "...\n")
  
  ### Get dataset and filter invalid coordinates ----
  df <- precomputed_data[[dataset_name]] %>%
    filter(
      !is.na(LATITUD),
      !is.na(LONGITUD),
      between(LATITUD, -4.23, 13.5),
      between(LONGITUD, -82.0, -66.87)
    )
  
  ### Validate and convert cantidad ----
  if (!"cantidad" %in% colnames(df)) {
    stop("Dataset ", dataset_name, " is missing critical 'cantidad' column. Processing halted.")
  }
  
  if (!is.numeric(df$cantidad)) {
    warning("In ", dataset_name, ": 'cantidad' is not numeric. Attempting conversion...")
    df <- df %>% 
      mutate(cantidad = as.numeric(cantidad))
  }
  
  ## Convert date if it exists ----
  if ("fecha_hecho" %in% colnames(df)) {
    tryCatch({
      df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho))
    }, error = function(e) {
      df$fecha_hecho <- as.Date(NA)
    })
  }
  
  ## Store cleaned dataset ----
  processed_data[[dataset_name]] <- df
  
  ## Create municipality list for this dataset -----
  if ("municipio" %in% colnames(df)) {
    municipality_list[[dataset_name]] <- unique(df$municipio)
  } else {
    municipality_list[[dataset_name]] <- character(0)
  }
  
  ## Create temporal aggregations if date exists ----
  if ("fecha_hecho" %in% colnames(df) && any(!is.na(df$fecha_hecho))) {
    # Monthly time series for overall timeline using cantidad
    monthly_counts <- df %>%
      filter(!is.na(fecha_hecho)) %>%
      mutate(month = floor_date(fecha_hecho, "month")) %>%
      group_by(month) %>%
      summarise(Acumulado = sum(cantidad, na.rm = TRUE)) %>%
      ungroup()
    
    ## Complete the time series with zeros for missing months -----
    if (nrow(monthly_counts) > 0) {
      date_range <- seq(min(monthly_counts$month), max(monthly_counts$month), by = "month")
      monthly_counts <- data.frame(month = date_range) %>%
        left_join(monthly_counts, by = "month") %>%
        mutate(Acumulado = replace_na(Acumulado, 0))
    }
    
    ## Municipality-month heatmap data using cantidad ----
    heatmap_data <- df %>%
      filter(!is.na(fecha_hecho)) %>%
      mutate(
        month = floor_date(fecha_hecho, "month"),
        municipio = as.character(municipio)
      ) %>%
      group_by(month, municipio) %>%
      summarise(total = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
      filter(total > 0) %>%
      arrange(month)
    
    ## Calculate top municipalities by cantidad ----
    if (nrow(heatmap_data) > 0) {
      top_munis <- heatmap_data %>%
        group_by(municipio) %>%
        summarise(total = sum(total)) %>%
        slice_max(total, n = 100) %>%
        pull(municipio)
      
      top_municipalities[[dataset_name]] <- top_munis
    } else {
      top_municipalities[[dataset_name]] <- character(0)
    }
    
    ## Store temporal aggregations ----
    temporal_aggregations[[dataset_name]] <- list(
      monthly_counts = monthly_counts,
      heatmap_data = heatmap_data
    )
  } else {
    temporal_aggregations[[dataset_name]] <- list(
      monthly_counts = data.frame(),
      heatmap_data = data.frame()
    )
    top_municipalities[[dataset_name]] <- character(0)
  }
  
  cat("Completed processing", dataset_name, "\n")
}

## Create metadata for caching ----
metadata <- list(
  creation_date = Sys.time(),
  datasets = names(precomputed_data),
  record_counts = sapply(processed_data, nrow)
)

# Save all preprocessed data -----
preprocessed_data <- list(
  processed_data = processed_data,
  municipality_list = municipality_list,
  temporal_aggregations = temporal_aggregations,
  top_municipalities = top_municipalities,
  metadata = metadata
)

saveRDS(preprocessed_data, "data/preprocessed_data.rds")
cat("Preprocessing completed and data saved.\n")