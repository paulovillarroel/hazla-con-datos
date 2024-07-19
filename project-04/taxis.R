# install.packages("duckdb")
library(duckdb)
library(tidyverse)
library(ggrepel)
library(tictoc)

# Definir la base de la URL
base_url <- "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_%d-%02d.parquet"

# Directorio de destino para guardar los archivos
dest_dir <- "project-04/raw-data"

# Crear una secuencia de meses
months <- 1:12
years <- 2013:2023

combinations <- expand.grid(year = years, month = months)

urls <- mapply(function(year, month) sprintf(base_url, year, month), 
       combinations$year, combinations$month)

# Descargar los archivos
map(urls, ~ download.file(url = .x, destfile = file.path(dest_dir, basename(.x)), mode = "wb"))

# sprintf(base_url, years, months)

# # Unir los archivos
# files <- list.files(path = dest_dir, pattern = "parquet$", full.names = TRUE)
# 
# # Leer los archivos
# taxis_df <- map(files, arrow::read_parquet) |>
#   bind_rows() |> 
#   select(-Airport_fee)

# Create a connection to an in-memory DuckDB database
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Create a table
#dbWriteTable(con, "taxis", taxis_df)

# Query the table
tbl(con, "read_parquet('project-04/raw-data/*.parquet', hive_partitioning = true)") |> 
  summarise(n = n()) |>
  collect()

tic()
tbl(con, "read_parquet('project-04/raw-data/*.parquet', hive_partitioning = true)") |> 
  filter(passenger_count > 0) |>
  summarise(mean_amount = mean(total_amount, na.rm = TRUE), 
            mean_distance = mean(trip_distance, na.rm = TRUE)) |> 
  collect()
toc()


tic()
tbl(con, "read_parquet('project-04/raw-data/*.parquet', hive_partitioning = true)") |> 
  filter(passenger_count > 0,
         year(tpep_pickup_datetime) %in% c(2013:2023)) |> 
  group_by(anio = year(tpep_pickup_datetime), time = hour(tpep_pickup_datetime) ) |> 
  count(time) |>
  collect() |>
  ggplot(aes(time, n, color = factor(anio))) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2.8) +
  labs(title = "Cantidad de viajes por hora - Yellow Taxi Trip New York (2013-2023)",
       x = "Hora (subida del pasajero)",
       y = "Cantidad de viajes",
       color = "Año",
       caption = "Fuente: https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 23, 2))
toc()


# Crear el gráfico con etiquetas en el último punto
tic()
data <- tbl(con, "read_parquet('project-04/raw-data/*.parquet', hive_partitioning = true)") |> 
  filter(passenger_count > 0,
         year(tpep_pickup_datetime) %in% c(2013:2023)) |> 
  group_by(anio = year(tpep_pickup_datetime), time = hour(tpep_pickup_datetime)) |> 
  count(time) |>
  collect()

# Encontrar los últimos puntos para cada año
last_points <- data |> 
  group_by(anio) |> 
  filter(time == max(time))

# Crear el gráfico
ggplot(data, aes(time, n, color = factor(anio))) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2.8) +
  geom_text_repel(data = last_points, aes(label = anio), size = 4, nudge_x = 1, hjust = 0) +
  labs(title = "Cantidad de viajes por hora - Yellow Taxi Trip New York (2013-2023)",
       x = "Hora (subida del pasajero)",
       y = "Cantidad de viajes",
       color = "Año",
       caption = "Fuente: https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme(legend.position = "none")
toc()
