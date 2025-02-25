#-------------------------- Basic packages-------------------------------

library(readxl)
library(tidyverse)
library(skimr)

#-------------------------------Data source--------------------------------------
#
# Source: https://www.mindefensa.gov.co/defensa-y-seguridad/datos-y-cifras/informacion-estadistica
# 
# Fecha de publicación de esta información: 16 de enero de 2025
# Fecha corte de la información publicada: a 31 de diciembre de 2024
#
# Avances en la lucha contra el problema mundial de las drogas (all 8 datasets)


#------------------Loading and storing data as a list of data frames ---------

# Specify the folder path
data_path <- "C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/raw-pub-16-enero-2025"

# List all files in the folder
xlsx_files <- list.files(path = data_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize a list to store data frames
data_frame_list <- list()

# Each xlsx into a df in the list
data_frame_list <- lapply(xlsx_files, read_xlsx)

# Name the list elements with the original file names (without extension)
names(data_frame_list) <- tools::file_path_sans_ext(basename(xlsx_files))

## Keeping the raw data untouched ----
working_data <- data_frame_list

## Saving working data as RDS----
saveRDS(working_data, "C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/working_data.rds")

## Saving working data as multiple CSV files---- 

dir.create("C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/working_data")
for (name in names(working_data)) {
  write.csv(working_data[[name]], 
            file = paste0("C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/working_data/", name, ".csv"), 
            row.names = FALSE)
}


#---------------------------General EDA-----------------------------------------

# Check the number of datasets in the list
length(working_data)

# Inspect the names of the datasets
names(working_data)

# Apply skim to each dataset in the list
skim_results <- lapply(working_data, skim)

# Save skim results to a text file
sink("skim_results.txt")
print(skim_results)
sink()

# Combine skim results into a single data frame
combined_skim_results <- bind_rows(skim_results, .id = "dataset_name")

# View the combined results
print(combined_skim_results)


#------------------------Transforming variables---------------------------------

### Common columns

# Extract column names from each data frame in the list
column_names_list <- lapply(working_data, names)

# Find common column names across all data frames
common_columns <- Reduce(intersect, column_names_list)

# Print the common column names across data frames
print(common_columns) # "FECHA HECHO"  "COD_DEPTO"    "DEPARTAMENTO" "COD_MUNI"     "MUNICIPIO"    "CANTIDAD" 


### Non-common column names across data frames

# All unique columns minus common columns
uncommon_cols <- setdiff(unique(unlist(column_names_list)), common_columns)
print(uncommon_cols) # "UNIDADES DE MEDIDA" "UNIDAD DE MEDIDA"   "TIPO CULTIVO" 


### Correcting column names for units of measurement across data frames

# Another approach to detect errors in the name of the column of units of measurement
# Shows dataset name along with its unique units

units_by_dataset <- lapply(working_data, function(df) {
  sort(unique(df$`UNIDADES DE MEDIDA`))
})
print(units_by_dataset)

# Standard name ---> UNIDAD

working_data <- lapply(working_data, function(df) {
  names(df)[names(df) %in% c("UNIDADES DE MEDIDA", "UNIDAD DE MEDIDA")] <- "UNIDAD"
  return(df)
})

units_clean <- lapply(working_data, function(df) {
  sort(unique(df$`UNIDAD`))
})

sink("units_clean.txt")
print(units_clean)
sink()



### Formatting all column names

library(janitor)

working_data <- lapply(working_data, function(df) {
  clean_names(df) # Convert column names to snake_case
})

# View column names for each df
lapply(working_data, colnames) 


### Types of crop eradicated 
working_data$ERRADICACIÓN$tipo_cultivo |> unique() # Hay error: "COCA"      "AMAPOLA"   "MARIHUANA" "Coca" 
table(working_data$ERRADICACIÓN$tipo_cultivo)


# Correction for type of crop
working_data$ERRADICACIÓN$tipo_cultivo <- gsub("Coca", "COCA", working_data$ERRADICACIÓN$tipo_cultivo, ignore.case = FALSE)

sink("tipos-cultivos-erradicados.txt")
table(working_data$ERRADICACIÓN$tipo_cultivo)
sink()


### Obs: hay más departamentos que los que existen en Colombia porque hay operaciones internacionales

### Obs: Numeric types, apparently all OK 


# Resumen de cantidad totales en el tiempo -----

library(dplyr)
library(lubridate)
library(scales)


# Function to calculate the total "cantidad" per "unidad" and the time range in years, months, and days
total_cantidad_per_df <- lapply(names(working_data), function(df_name) {
  df <- working_data[[df_name]]  # Extract the data frame
  
  if (all(c("cantidad", "unidad", "fecha_hecho") %in% names(df))) {
    
    # Convert fecha_hecho to Date if not already
    df <- df %>% mutate(fecha_hecho = as.Date(fecha_hecho)) 
    
    # Calculate min and max date
    min_date <- min(df$fecha_hecho, na.rm = TRUE)
    max_date <- max(df$fecha_hecho, na.rm = TRUE)
    
    # Compute the time difference in years, months, and days
    time_diff <- as.period(interval(start = min_date, end = max_date))
    time_range <- paste(time_diff@year, "years,", time_diff@month, "months,", time_diff@day, "days")
    
    # Group by "unidad" and summarize "cantidad"
    df_summary <- df %>%
      group_by(unidad) %>%
      summarise(total_cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
      mutate(dataframe = df_name, 
             min_date = min_date,
             max_date = max_date,
             time_range = time_range)  # Add time range columns
    
    return(df_summary)
  } else {
    NULL  # Ignore data frames without required columns
  }
}) %>%
  bind_rows()  # Combine all results into one data frame


# Re-arranging the column order

total_cantidad_per_df <- total_cantidad_per_df %>%
  mutate(total_cantidad = comma(total_cantidad)) %>%
  select(dataframe, total_cantidad, unidad, min_date, max_date, time_range)


# Print the result
sink("resumen-totales-tiempo.txt")
print(total_cantidad_per_df)
sink()



#-----------------------Geographical data--------------------------------------



# Total cases at...

### Departmento level 

sink("casos-departamentos.txt")
# Iterate over all dataframes in the working_data list
for (df_name in names(working_data)) {
  # Check if the dataframe has a "departamento" column
  if ("departamento" %in% colnames(working_data[[df_name]])) {
    # Calculate the frequency table for the "departamento" column
    freq_table <- table(working_data[[df_name]]$departamento)
    
    # Print the name of the dataframe and its frequency table
    cat("Frequency table for", df_name, ":\n")
    print(freq_table)
    cat("\n")  # Add a newline for better readability
  } else {
    # If the dataframe doesn't have a "departamento" column, print a message
    cat("Dataframe", df_name, "does not have a 'departamento' column.\n\n")
  }
}
sink()



### Municipio level

sink("casos-municipios.txt")
for (df_name in names(working_data)) {
  # Check if the dataframe has a "departamento" column
  if ("municipio" %in% colnames(working_data[[df_name]])) {
    # Calculate the frequency table for the "departamento" column
      freq_table <- table(working_data[[df_name]]$municipio)
    
    # Print the name of the dataframe and its frequency table
    cat("Frequency table for", df_name, ":\n")
    print(freq_table)
    cat("\n")  # Add a newline for better readability
  } else {
    # If the dataframe doesn't have a "municipio" column, print a message
    cat("Dataframe", df_name, "does not have a 'municipio' column.\n\n")
  }
}
sink()

### DIVIPOLA ----
# https://www.datos.gov.co/Mapas-Nacionales/DIVIPOLA-C-digos-municipios-geolocalizados/vafm-j2df/about_data

divipola <- read.csv("C:/Users/USER/Desktop/codebaker/all_r/lucha-drogas/data/divipola.csv")
skim(divipola)
divipola$COD_MPIO <- gsub(",", "", divipola$COD_MPIO)



# How many municipalities by department are there? 

sink("municipios-por-departamento.txt")  # Open the sink to write to a file
divipola %>%
  group_by(NOM_DPTO) %>%
  summarise(unique_COD_MPIO = n_distinct(COD_MPIO)) %>%
  arrange(desc(unique_COD_MPIO)) %>%  # Sort in descending order
  print(n=Inf)
sink()  

# Removing the first 0 from cod_muni in working_data list of data frames

remove_first_zero <- function(df) {
  df %>%
    mutate(cod_muni = sub("^0", "", cod_muni))  # Remove the first 0
}

## Merge ----

# Step 1:Apply the function to each data frame in the working_data list
working_data <- lapply(working_data, remove_first_zero)

# Step 2: Define a function to perform the merge for a single data frame
merge_with_divipola <- function(df) {
  df %>%
    left_join(divipola %>% select(COD_MPIO, LATITUD, LONGITUD, Geo.Municipio), 
              by = c("cod_muni" = "COD_MPIO"))
}

# Step 3: Apply the function to all data frames in the working_data list
working_data <- lapply(working_data, merge_with_divipola)


#-----------------Geo transformations and completions--------------------------

sink("skim_geo.txt")
lapply(working_data, skim)
sink()

### NAs 

library(naniar) #
lapply(working_data, gg_miss_var)

## NAs in a data frame----

find_na_rows <- function(df) {
  df %>%
    filter(rowSums(is.na(.)) > 0)  # Filter rows with at least one NA
}

# Apply the function to each data frame in the working_data list
na_rows_list <- lapply(working_data, find_na_rows)

# Print the rows with NAs for each data frame
for (i in seq_along(na_rows_list)) {
  df_name <- names(working_data)[i]  # Get the name of the data frame
  cat("Rows with NAs in", df_name, ":\n")
  print(na_rows_list[[i]])
  cat("\n")
}


# Obs: ASPERSION, INCAUTACIÓN DE BASUCO sin NAs


# Resumen lugares sin geolocalización y por tanto NAs en working_data-----

sink("missing-places.txt")
lapply(na_rows_list, function(df) {
  # Check if the "municipio" column exists in the data frame
  if ("municipio" %in% colnames(df)) {
    # Extract unique municipios
    unique(df[["municipio"]])
  } else {
    # If "municipio" column doesn't exist, return NULL or a message
    NULL
  }
})
sink()

###------------------ Let's geo!-------------------------------------------------

# Modificación de data frames con lugares sin geodata

####---1. DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS`----

# "SIN ESTABLECER" "VENEZUELA"      "MAPIRIPANA"     "PERU" 

# Obs: Mapiripana fue integrada al municipio de Barrancominas, Guainía en 2019
# https://www.elpais.com.co/colombia/barrancominas-fue-formalizado-como-nuevo-municipio-del-guainia.html

# Obs: Donde hay otros países como municipios, se trata como un solo lugar (país) y con respectiva geo data, 
# sin importar diferentes códigos dentro de esos países. 


# Update the dataframe based on specific municipio values


working_data[["DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS"]] <- 
  working_data[["DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "SIN ESTABLECER" ~ NA_real_,  # Use NA_real_ for numeric columns
      municipio == "VENEZUELA" ~ 10.48801,
      municipio == "MAPIRIPANA" ~ 2.776245,
      municipio == "PERU" ~ -9.189967,
      TRUE ~ LATITUD  # Keep the original value if no match
    ),
    LONGITUD = case_when(
      municipio == "SIN ESTABLECER" ~ NA_real_,  # Use NA_real_ for numeric columns
      municipio == "VENEZUELA" ~ -66.87919,
      municipio == "MAPIRIPANA" ~ -70.447335,
      municipio == "PERU" ~ -75.015152,
      TRUE ~ LONGITUD  # Keep the original value if no match
    ),
    Geo.Municipio = case_when(
      municipio == "SIN ESTABLECER" ~ "SIN ESTABLECER",
      municipio == "VENEZUELA" ~ "VENEZUELA",
      municipio == "MAPIRIPANA" ~ "MAPIRIPANA",
      municipio == "PERU" ~ "PERU",
      TRUE ~ Geo.Municipio  # Keep the original value if no match
    )
  )

# REMOVAL: 1 row where municipio == SIN ESTABLECER con poca cantidad (aunque es también es cierto que no sabemos su importancia)
na_rows_list[["DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS"]]

working_data[["DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS"]] <- 
  working_data[["DESTRUCCIÓN INFRAESTRUCTURAS PARA LA PRODUCCIÓN DE DROGAS ILÍCITAS"]] %>% filter(municipio != "SIN ESTABLECER")

####---2. ERRADICACIÓN----

#"PUERTO ASIS" (0.500556, -76.4989) "NARIÑO"  (5.60917, -75.1764)    "SAN CARLOS" (6.18722, -74.9922)  
# "SAN LUIS" ( Latitud: 6.04222 ; Longitud: -74.9933)   "EL CHARCO"(Latitud: 2.4775 ; Longitud: -78.1111 )  
# "SAN MIGUEL"(0.343611. Longitud: -76.9108)

working_data[["ERRADICACIÓN"]][["municipio"]] |> unique() |> sort()
na_rows_list[["ERRADICACIÓN"]]
table(working_data$ERRADICACIÓN$municipio)

working_data[["ERRADICACIÓN"]] <- 
  working_data[["ERRADICACIÓN"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "PUERTO ASIS" ~ 0.500556,
      municipio == "NARIÑO" ~ 5.60917,
      municipio == "SAN CARLOS" ~ 6.18722,
      municipio == "SAN LUIS" ~ 6.04222,
      municipio == "EL CHARCO" ~ 2.4775,
      municipio == "SAN MIGUEL" ~ 0.343611,
      TRUE ~ LATITUD  # Keep the original value if no match
    ),
    LONGITUD = case_when(
      municipio == "PUERTO ASIS" ~ -76.4989,
      municipio == "NARIÑO" ~ -75.1764,
      municipio == "SAN CARLOS" ~ -74.9922,
      municipio == "SAN LUIS" ~ -74.9933,
      municipio == "EL CHARCO" ~ -78.1111,
      municipio == "SAN MIGUEL" ~ -76.9108,
      TRUE ~ LONGITUD  # Keep the original value if no match
    ),
    Geo.Municipio = case_when(
      municipio == "PUERTO ASIS" ~ "PUERTO ASIS",
      municipio == "NARIÑO" ~ "NARIÑO",
      municipio == "SAN CARLOS" ~ "SAN CARLOS",
      municipio == "SAN LUIS" ~ "SAN LUIS",
      municipio == "EL CHARCO" ~ "EL CHARCO",
      municipio == "SAN MIGUEL" ~ "SAN MIGUEL",
      TRUE ~ Geo.Municipio  # Keep the original value if no match
    )
  )

# Removal: 8 rows due to NAs in cantidad

any(is.na(working_data[["ERRADICACIÓN"]]))
sum(is.na(working_data[["ERRADICACIÓN"]]))
colSums(is.na(working_data[["ERRADICACIÓN"]]))


working_data[["ERRADICACIÓN"]] <- 
  working_data[["ERRADICACIÓN"]] %>%  drop_na()


####----3. INCAUTACIÓN DE BASE DE COCA----
# "PERU"                  "MAPIRIPANA"            "BRASIL"                "CHILE"                 "ESPAÑA"               
# "AGUAS INTERNACIONALES" "ECUADOR"  

working_data[["INCAUTACIÓN DE BASE DE COCA"]][["municipio"]] |> unique() |> sort()
table(working_data$`INCAUTACIÓN DE BASE DE COCA`$municipio) |> sort()
na_rows_list[["INCAUTACIÓN DE BASE DE COCA"]]


# Update the dataframe with additional countries' coordinates
working_data[["INCAUTACIÓN DE BASE DE COCA"]] <- 
  working_data[["INCAUTACIÓN DE BASE DE COCA"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "MAPIRIPANA" ~ 2.776245,
      municipio == "PERU" ~ -9.189967,
      municipio == "BRASIL" ~ -14.2350,  # Latitude for BRASIL
      municipio == "CHILE" ~ -35.6751,   # Latitude for CHILE
      municipio == "ESPAÑA" ~ 40.4637,   # Latitude for ESPAÑA
      municipio == "AGUAS INTERNACIONALES" ~ 10.017320,  # No specific latitude for international waters
      municipio == "ECUADOR" ~ -1.8312,  # Latitude for ECUADOR
      TRUE ~ LATITUD  # Keep the original value if no match
    ),
    LONGITUD = case_when(
      municipio == "MAPIRIPANA" ~ -70.447335,
      municipio == "PERU" ~ -75.015152,
      municipio == "BRASIL" ~ -51.9253,  # Longitude for BRASIL
      municipio == "CHILE" ~ -71.5430,   # Longitude for CHILE
      municipio == "ESPAÑA" ~ -3.7492,   # Longitude for ESPAÑA
      municipio == "AGUAS INTERNACIONALES" ~ -80.916803,  # No specific longitude for international waters
      municipio == "ECUADOR" ~ -78.1834, # Longitude for ECUADOR
      TRUE ~ LONGITUD  # Keep the original value if no match
    ),
    Geo.Municipio = case_when(
      municipio == "MAPIRIPANA" ~ "MAPIRIPANA",
      municipio == "PERU" ~ "PERU",
      municipio == "BRASIL" ~ "BRASIL",
      municipio == "CHILE" ~ "CHILE",
      municipio == "ESPAÑA" ~ "ESPAÑA",
      municipio == "AGUAS INTERNACIONALES" ~ "AGUAS INTERNACIONALES",
      municipio == "ECUADOR" ~ "ECUADOR",
      TRUE ~ Geo.Municipio  # Keep the original value if no match
    ), 
    cod_muni = case_when(
      municipio == "CHILE" ~ 0004, # porque sí 
      municipio == "AGUAS INTERNACIONALES" ~ 0005, # porque sí
      TRUE ~ LONGITUD
      ), 
    cod_depto = case_when(
      municipio == "CHILE" ~ 0004,# porque sí
      municipio == "AGUAS INTERNACIONALES" ~ 0005,# porque sí
      TRUE ~ LONGITUD
    )
    
    )

any(is.na(working_data[["INCAUTACIÓN DE BASE DE COCA"]]))
sum(is.na(working_data[["INCAUTACIÓN DE BASE DE COCA"]]))
colSums(is.na(working_data[["INCAUTACIÓN DE BASE DE COCA"]]))


# Obs: aguas internacionales ponerle lat y long, es preocupante que ni siquiera se distinga entre océanos...
# Finalmente elijo un punto en el Atlántico (Frente a Panamá).

####--- 4. INCAUTACIÓN DE COCAÍNA ----

working_data[["INCAUTACIÓN DE COCAINA"]][["municipio"]] |> unique() |> sort()
table(working_data$`INCAUTACIÓN DE COCAINA`$municipio) |> sort()

sum(na_rows_list[["INCAUTACIÓN DE COCAINA"]]$cantidad)
sum(working_data[["INCAUTACIÓN DE COCAINA"]]$cantidad)

any(is.na(working_data[["INCAUTACIÓN DE COCAINA"]]))
sum(is.na(working_data[["INCAUTACIÓN DE COCAINA"]]))
colSums(is.na(working_data[["INCAUTACIÓN DE COCAINA"]]))



# REMOVAL 26 cases SIN ESTABLECER

working_data[["INCAUTACIÓN DE COCAINA"]] %>% 
  summarise(count = sum(municipio == "SIN ESTABLECER", na.rm = TRUE)) %>% 
  pull(count) # 26 casos de lugares SIN ESTABLECER 

working_data[["INCAUTACIÓN DE COCAINA"]] %>% filter(municipio == "SIN ESTABLECER")

working_data[["INCAUTACIÓN DE COCAINA"]] %>% 
  filter(municipio == "SIN ESTABLECER") %>% 
  summarise(total = sum(cantidad, na.rm = TRUE)) %>% 
  pull(total) # 20,344.9 kg en total "SIN ESTABLECER" en todo el periodo de tiempo para incautación de cocaína.

# Drop SIN ESTABLECER
working_data[["INCAUTACIÓN DE COCAINA"]] <- working_data[["INCAUTACIÓN DE COCAINA"]] %>% 
  filter(municipio != "SIN ESTABLECER")


##------------ Dropping unnecessary columns ----------

# Actualización de NAs
na_rows_list <- lapply(working_data, find_na_rows)


# "cod_depto", "cod_muni", Geo.Municipio

cols_to_drop <- c("cod_depto", "cod_muni", "Geo.Municipio")

working_data <- lapply(working_data, function(df) {
  df %>% select(-any_of(cols_to_drop))
})

# Actualización de NAs
na_rows_list <- lapply(working_data, find_na_rows)


# Coordenadas del del resto, junto con cod_muni y cod_depto

# Correción: EEUU

table(working_data$`INCAUTACIÓN DE COCAINA`$municipio)

working_data$`INCAUTACIÓN DE COCAINA`$municipio <- gsub("ESTADOS UNIDOS DE AMERICA", "EEUU", working_data$`INCAUTACIÓN DE COCAINA`$municipio, ignore.case = FALSE)
working_data$`INCAUTACIÓN DE COCAINA`$municipio <- gsub("ESTADOS UNIDOS", "EEUU", working_data$`INCAUTACIÓN DE COCAINA`$municipio, ignore.case = FALSE)


# Correción: ANTILLAS NEERLANDESAS

working_data$`INCAUTACIÓN DE COCAINA`$municipio <- gsub("ANTILLAS HOLANDESAS", "ANTILLAS NEERLANDESAS", working_data$`INCAUTACIÓN DE COCAINA`$municipio, ignore.case = FALSE)


# Correción: PAISES BAJOS

working_data$`INCAUTACIÓN DE COCAINA`$municipio <- gsub("HOLANDA", "PAISES BAJOS", working_data$`INCAUTACIÓN DE COCAINA`$municipio, ignore.case = FALSE)


working_data[["INCAUTACIÓN DE COCAINA"]] <- 
  working_data[["INCAUTACIÓN DE COCAINA"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "MIAMI" ~ 25.7617,
      municipio == "PANAMA" ~ 8.9824,
      municipio == "ECUADOR" ~ -1.8312,
      municipio == "MADRID" ~ 40.4168,
      municipio == "JAMAICA" ~ 18.1096,
      municipio == "BELGICA" ~ 50.8503,
      municipio == "COSTA RICA" ~ 9.7489,
      municipio == "NICARAGUA" ~ 12.8654,
      municipio == "GUATEMALA" ~ 15.7835,
      municipio == "HONDURAS" ~ 15.2000,
      municipio == "MAR PACIFICO" ~ 0.0000,
      municipio == "INGLATERRA" ~ 52.3555,
      municipio == "ITALIA" ~ 41.8719,
      municipio == "AARHUS" ~ 56.1629,
      municipio == "MEXICO" ~ 23.6345,
      municipio == "VENEZUELA" ~ 6.4238,
      municipio == "PERU" ~ -9.1900,
      municipio == "REPUBLICA DOMINICANA" ~ 18.7357,
      municipio == "EL SALVADOR" ~ 13.7942,
      municipio == "FRANCIA" ~ 46.6034,
      municipio == "BRASIL" ~ -14.2350,
      municipio == "ANTILLAS NEERLANDESAS" ~ 12.1224,
      municipio == "PUERTO RICO" ~ 18.2208,
      municipio == "ALBANIA" ~ 41.1533,
      municipio == "ALEMANIA" ~ 51.1657,
      municipio == "PORTUGAL" ~ 39.3999,
      municipio == "ARUBA" ~ 12.5211,
      municipio == "BELICE" ~ 17.1899,
      municipio == "CHINA" ~ 35.8617,
      municipio == "AGUAS INTERNACIONALES" ~ 0.0000,
      municipio == "SUDAFRICA" ~ -30.5595,
      municipio == "PARAGUAY" ~ -23.4425,
      municipio == "RUMANIA" ~ 45.9432,
      municipio == "CROACIA" ~ 45.1000,
      municipio == "ESPAÑA" ~ 40.4637,
      municipio == "AGUAS JURISDICCIONALES" ~ 25.0000,
      municipio == "EEUU" ~ 37.0902,
      municipio == "PAISES BAJOS" ~ 52.3676,
      municipio == "DOVER" ~ 51.1290,
      municipio == "CANADA" ~ 56.1304,
      municipio == "MACEDONIA" ~ 41.6086,
      municipio == "MALTA" ~ 35.8997,
      municipio == "CURAZAO" ~ 12.1696,
      municipio == "LA HABANA" ~ 23.1136,
      municipio == "BAHAMAS" ~ 25.0343,
      municipio == "CHILE" ~ -35.6751,
      municipio == "SURINAN" ~ 5.8394,
      municipio == "UCRANIA" ~ 48.3794,
      municipio == "REINO UNIDO" ~ 55.3781,
      municipio == "ISLAS VIRGENES BRITANICAS" ~ 18.4207,
      municipio == "TURQUIA" ~ 38.9637,
      municipio == "GUINEA" ~ 9.9456,
      municipio == "SURINAM" ~ 5.8394,
      municipio == "CABO VERDE" ~ 16.5388,
      municipio == "RUSIA" ~ 61.5240,
      municipio == "LIBERIA" ~ 6.4281,
      municipio == "GUAYANA" ~ 4.8604,
      municipio == "MARTINICA" ~ 14.6415,
      municipio == "PUERTO VLISSINGEN" ~ 51.4425,
      municipio == "ARGENTINA" ~ -38.4161,
      municipio == "GUINEA BISSAU" ~ 11.8037,
      municipio == "URUGUAY" ~ -32.5228,
      municipio == "SIN ESTABLECER" ~ 0.0000,
      TRUE ~ LATITUD
    ),
    
    LONGITUD = case_when(
      municipio == "MIAMI" ~ -80.1918,
      municipio == "PANAMA" ~ -79.5199,
      municipio == "ECUADOR" ~ -78.1834,
      municipio == "MADRID" ~ -3.7038,
      municipio == "JAMAICA" ~ -77.2975,
      municipio == "BELGICA" ~ 4.3517,
      municipio == "COSTA RICA" ~ -83.7534,
      municipio == "NICARAGUA" ~ -85.2072,
      municipio == "GUATEMALA" ~ -90.2308,
      municipio == "HONDURAS" ~ -86.2419,
      municipio == "MAR PACIFICO" ~ 0.0000,
      municipio == "HOLANDA" ~ 5.2913,
      municipio == "INGLATERRA" ~ -1.1743,
      municipio == "ITALIA" ~ 12.5674,
      municipio == "AARHUS" ~ 10.2039,
      municipio == "MEXICO" ~ -102.5528,
      municipio == "VENEZUELA" ~ -66.5897,
      municipio == "PERU" ~ -75.0152,
      municipio == "REPUBLICA DOMINICANA" ~ -70.1627,
      municipio == "EL SALVADOR" ~ -89.2073,
      municipio == "FRANCIA" ~ 2.2137,
      municipio == "BRASIL" ~ -51.9253,
      municipio == "ANTILLAS NEERLANDESAS" ~ -68.9335,
      municipio == "PUERTO RICO" ~ -66.5901,
      municipio == "ALBANIA" ~ 20.1683,
      municipio == "ALEMANIA" ~ 10.4515,
      municipio == "PORTUGAL" ~ -8.2245,
      municipio == "ARUBA" ~ -69.9683,
      municipio == "BELICE" ~ -88.4976,
      municipio == "CHINA" ~ 104.1954,
      municipio == "AGUAS INTERNACIONALES" ~ 0.0000,
      municipio == "SUDAFRICA" ~ 22.9375,
      municipio == "PARAGUAY" ~ -58.4438,
      municipio == "RUMANIA" ~ 24.9668,
      municipio == "CROACIA" ~ 15.2000,
      municipio == "ESPAÑA" ~ -3.7492,
      municipio == "AGUAS JURISDICCIONALES" ~ -50.0000,
      municipio == "EEUU" ~ -95.7129,
      municipio == "PAISES BAJOS" ~ 4.9041,
      municipio == "DOVER" ~ 1.3116,
      municipio == "CANADA" ~ -106.3468,
      municipio == "MACEDONIA" ~ 21.7453,
      municipio == "MALTA" ~ 14.3754,
      municipio == "CURAZAO" ~ -69.0200,
      municipio == "LA HABANA" ~ -82.3666,
      municipio == "BAHAMAS" ~ -77.3963,
      municipio == "CHILE" ~ -71.5429,
      municipio == "SURINAN" ~ -56.0278,
      municipio == "UCRANIA" ~ 31.1656,
      municipio == "REINO UNIDO" ~ -3.4360,
      municipio == "ISLAS VIRGENES BRITANICAS" ~ -64.6399,
      municipio == "TURQUIA" ~ 35.2433,
      municipio == "GUINEA" ~ -11.9041,
      municipio == "SURINAM" ~ -56.0278,
      municipio == "CABO VERDE" ~ -23.0418,
      municipio == "RUSIA" ~ 105.3188,
      municipio == "LIBERIA" ~ -9.4295,
      municipio == "GUAYANA" ~ -58.9302,
      municipio == "MARTINICA" ~ -61.0242,
      municipio == "PUERTO VLISSINGEN" ~ 3.5899,
      municipio == "ARGENTINA" ~ -63.6167,
      municipio == "GUINEA BISSAU" ~ -15.1804,
      municipio == "URUGUAY" ~ -55.7658,
      municipio == "SIN ESTABLECER" ~ 0.0000,
      TRUE ~ LONGITUD
    )
  )


na_rows_list <- lapply(working_data, find_na_rows)

#### 5. INCAUTACIÓN DE HEROÍNA --------------

# [1] "SIN ESTABLECER"            "MIAMI"                     "COSTA RICA"                "PANAMA"                   
# [5] "GUATEMALA"                 "ECUADOR"                   "ESTADOS UNIDOS"            "AGUAS INTERNACIONALES"    
# [9] "ESTADOS UNIDOS DE AMERICA"



working_data[["INCAUTACIÓN DE HEROINA"]][["municipio"]] |> unique() |> sort()
table(working_data$`INCAUTACIÓN DE HEROINA`$municipio) |> sort()

sum(na_rows_list[["INCAUTACIÓN DE HEROINA"]]$cantidad)
sum(working_data[["INCAUTACIÓN DE HEROINA"]]$cantidad)

any(is.na(working_data[["INCAUTACIÓN DE HEROINA"]]))
sum(is.na(working_data[["INCAUTACIÓN DE HEROINA"]]))
colSums(is.na(working_data[["INCAUTACIÓN DE HEROINA"]]))




working_data$`INCAUTACIÓN DE HEROINA`$municipio <- gsub("ESTADOS UNIDOS DE AMERICA", "EEUU", working_data$`INCAUTACIÓN DE HEROINA`$municipio, ignore.case = FALSE)
working_data$`INCAUTACIÓN DE HEROINA`$municipio <- gsub("ESTADOS UNIDOS", "EEUU", working_data$`INCAUTACIÓN DE HEROINA`$municipio, ignore.case = FALSE)

# REMOVAL: SIN ESTABLECER

working_data[["INCAUTACIÓN DE HEROINA"]] %>% 
  summarise(count = sum(municipio == "SIN ESTABLECER", na.rm = TRUE)) %>% 
  pull(count) # 7 casos de lugares SIN ESTABLECER 

working_data[["INCAUTACIÓN DE HEROINA"]] %>% filter(municipio == "SIN ESTABLECER")

working_data[["INCAUTACIÓN DE HEROINA"]] %>% 
  filter(municipio == "SIN ESTABLECER") %>% 
  summarise(total = sum(cantidad, na.rm = TRUE)) %>% 
  pull(total) # 289.84 kg en total "SIN ESTABLECER" en todo el periodo de tiempo para incautación de heroína.


working_data[["INCAUTACIÓN DE HEROINA"]] <- working_data[["INCAUTACIÓN DE HEROINA"]] %>% 
  filter(municipio != "SIN ESTABLECER")


##
working_data[["INCAUTACIÓN DE HEROINA"]] <- 
  working_data[["INCAUTACIÓN DE HEROINA"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "MIAMI" ~ 25.7617,
      municipio == "COSTA RICA" ~ 9.7489,
      municipio == "PANAMA" ~ 8.9833,
      municipio == "GUATEMALA" ~ 15.7835,
      municipio == "ECUADOR" ~ -1.8312,
      municipio == "EEUU" ~ 37.0902,  # EEUU is a broad term; no single lat/lon
      municipio == "AGUAS INTERNACIONALES" ~ 0.00,  # Undefined location
      TRUE ~ LATITUD  
    ),
    LONGITUD = case_when(
      municipio == "MIAMI" ~ -80.1918,
      municipio == "COSTA RICA" ~ -83.7534,
      municipio == "PANAMA" ~ -79.5167,
      municipio == "GUATEMALA" ~ -90.2308,
      municipio == "ECUADOR" ~ -78.1834,
      municipio == "EEUU" ~ -95.7129,
      municipio == "AGUAS INTERNACIONALES" ~ 0.00,
      TRUE ~ LONGITUD  
    )
  )


na_rows_list <- lapply(working_data, find_na_rows)


#### 6. INCAUTACIÓN DE MARIHUANA ------------

# [1] "PANAMA"                    "MAR PACIFICO"              "COSTA RICA"                "VENEZUELA"                
# [5] "BRASIL"                    "PERU"                      "CHILE"                     "HONDURAS"                 
# [9] "ECUADOR"                   "MADRID"                    "AGUAS INTERNACIONALES"     "REPUBLICA DOMINICANA"     
# [13] "ARUBA"                     "FRANCIA"                   "ESPAÑA"                    "BOLIVIA"                  
# [17] "ESTADOS UNIDOS DE AMERICA" "PUERTO VLISSINGEN"         "ANTILLAS NEERLANDESAS"     "BAHAMAS"


working_data[["INCAUTACIÓN DE MARIHUANA"]][["municipio"]] |> unique() |> sort()
table(working_data$`INCAUTACIÓN DE MARIHUANA`$municipio) |> sort()

sum(na_rows_list[["INCAUTACIÓN DE MARIHUANA"]]$cantidad)
sum(working_data[["INCAUTACIÓN DE MARIHUANA"]]$cantidad)

any(is.na(working_data[["INCAUTACIÓN DE MARIHUANA"]]))
sum(is.na(working_data[["INCAUTACIÓN DE MARIHUANA"]]))
colSums(is.na(working_data[["INCAUTACIÓN DE MARIHUANA"]]))


# Correciones de nombres de países


working_data$`INCAUTACIÓN DE MARIHUANA`$municipio <- gsub("ESTADOS UNIDOS DE AMERICA", "EEUU", working_data$`INCAUTACIÓN DE MARIHUANA`$municipio, ignore.case = FALSE)



working_data[["INCAUTACIÓN DE MARIHUANA"]] <- 
  working_data[["INCAUTACIÓN DE MARIHUANA"]] %>%
  mutate(
    LATITUD = case_when(
      municipio == "PANAMA" ~ 8.9833,
      municipio == "MAR PACIFICO" ~ 0.00,  # Undefined oceanic location
      municipio == "COSTA RICA" ~ 9.7489,
      municipio == "VENEZUELA" ~ 6.4238,
      municipio == "BRASIL" ~ -14.2350,
      municipio == "PERU" ~ -9.1900,
      municipio == "CHILE" ~ -35.6751,
      municipio == "HONDURAS" ~ 15.2000,
      municipio == "ECUADOR" ~ -1.8312,
      municipio == "MADRID" ~ 40.4168,
      municipio == "AGUAS INTERNACIONALES" ~ 0.00,  # Undefined location
      municipio == "REPUBLICA DOMINICANA" ~ 18.7357,
      municipio == "ARUBA" ~ 12.5211,
      municipio == "FRANCIA" ~ 46.6034,
      municipio == "ESPAÑA" ~ 40.4637,
      municipio == "BOLIVIA" ~ -16.2902,
      municipio == "EEUU" ~ 37.0902,  #
      municipio == "PUERTO VLISSINGEN" ~ 51.4425,
      municipio == "ANTILLAS NEERLANDESAS" ~ 12.2261,  # Approximate for former Dutch Antilles
      municipio == "BAHAMAS" ~ 25.0343,
      TRUE ~ LATITUD  
    ),
    LONGITUD = case_when(
      municipio == "PANAMA" ~ -79.5167,
      municipio == "MAR PACIFICO" ~ 0.00,
      municipio == "COSTA RICA" ~ -83.7534,
      municipio == "VENEZUELA" ~ -66.5897,
      municipio == "BRASIL" ~ -51.9253,
      municipio == "PERU" ~ -75.0152,
      municipio == "CHILE" ~ -71.5430,
      municipio == "HONDURAS" ~ -86.2419,
      municipio == "ECUADOR" ~ -78.1834,
      municipio == "MADRID" ~ -3.7038,
      municipio == "AGUAS INTERNACIONALES" ~ 0.00,
      municipio == "REPUBLICA DOMINICANA" ~ -70.1627,
      municipio == "ARUBA" ~ -69.9683,
      municipio == "FRANCIA" ~ 1.8883,
      municipio == "ESPAÑA" ~ -3.7492,
      municipio == "BOLIVIA" ~ -63.5887,
      municipio == "EEUU" ~ -95.7129,
      municipio == "PUERTO VLISSINGEN" ~ 3.5736,
      municipio == "ANTILLAS NEERLANDESAS" ~ -69.0600,
      municipio == "BAHAMAS" ~ -77.3963,
      TRUE ~ LONGITUD  
    )
  )


na_rows_list <- lapply(working_data, find_na_rows)

