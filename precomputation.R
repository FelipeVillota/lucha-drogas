# Libraries ----
library(tidyverse)
library(lubridate)

# Load working data ----
working_data <- readRDS("data/working_data/working_data.rds") 

# Create preprocessed data structures ----
processed_data <- list()
municipality_list <- list()
temporal_aggregations <- list()
top_municipalities <- list()

## Main processing loop ----
for (dataset_name in names(working_data)) {
  cat("Processing", dataset_name, "...\n")

  ### Get dataset and filter invalid coordinates ----
  df <- working_data[[dataset_name]] %>%
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
      mutate(cantidad = parse_number(as.character(cantidad))) # More robust conversion
  }

  ## Convert date if it exists ----
  if ("fecha_hecho" %in% colnames(df)) {
    df <- df %>%
      mutate(fecha_hecho = suppressWarnings(as.Date(fecha_hecho, format = "%Y-%m-%d")))
  }

  ## Store cleaned dataset ----
  processed_data[[dataset_name]] <- df

  ## Municipality list for this dataset -----
  municipality_list[[dataset_name]] <- if ("municipio" %in% colnames(df)) unique(df$municipio) else character(0)

  ## Temporal aggregations if date exists ----
  if ("fecha_hecho" %in% colnames(df) && any(!is.na(df$fecha_hecho))) {

    ### Count of events (rows) per month ----

    monthly_counts <- df %>%
      filter(!is.na(fecha_hecho)) %>%
      mutate(month = floor_date(fecha_hecho, "month")) %>%
      count(month, name = "Operaciones")

    # Handle missing months

    if (nrow(monthly_counts) > 0) {
      date_range <- seq(min(monthly_counts$month, na.rm = TRUE), max(monthly_counts$month, na.rm = TRUE), by = "month")
      monthly_counts <- tibble(month = date_range) %>%
        left_join(monthly_counts, by = "month") %>%
        mutate(Operaciones = replace_na(Operaciones, 0)) %>%
        arrange(month) %>%
        mutate(Cumulative_Operaciones = cumsum(Operaciones)) # Cumulative event count
    }

    ## Sum of cantidad intervenida per month ----


    cantidad_sums <- df %>%
      filter(!is.na(fecha_hecho)) %>%
      mutate(month = floor_date(fecha_hecho, "month")) %>%
      group_by(month) %>%
      summarise(Cantidad = sum(cantidad, na.rm = TRUE)) %>%
      ungroup()

    # Handle missing months
    if (nrow(cantidad_sums) > 0) {
      date_range <- seq(min(cantidad_sums$month, na.rm = TRUE),
                        max(cantidad_sums$month, na.rm = TRUE),
                        by = "month")
      cantidad_sums <- tibble(month = date_range) %>%
        left_join(cantidad_sums, by = "month") %>%
        mutate(Cantidad = replace_na(Cantidad, 0)) %>%
        arrange(month) %>%
        mutate(Cumulative_Cantidad = cumsum(Cantidad))
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
      cantidad_sums = cantidad_sums,  # Now correctly stored
      heatmap_data = heatmap_data
    )
  } else {
    temporal_aggregations[[dataset_name]] <- list(
      monthly_counts = data.frame(),
      cantidad_sums = data.frame(),
      heatmap_data = data.frame()
    )
    top_municipalities[[dataset_name]] <- character(0)
  }

  cat("Completed processing", dataset_name, "\n")
}

## Create metadata for caching ----
metadata <- list(
  creation_date = Sys.time(),
  datasets = names(working_data),
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
