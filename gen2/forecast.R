library(tidyverse)
library(arrow)
library(janitor)
library(forecast)
library(tsfgrnn)

url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

data <- read_parquet(url)

data <- data |> 
  janitor::clean_names()

# Eliminar última semana epidemiológica
ultima_semana <- data |>
  filter(anio == 2024) |>
  distinct(semana_estadistica) |>
  arrange(semana_estadistica) |>
  pull(semana_estadistica) |>
  last()

data <- data |>
  filter(!(anio == 2024 & semana_estadistica == ultima_semana))

# Filtrar casos neumonías
casos_neumonia <- data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |>
  summarise(total_casos = sum(num_total), .by = c(anio, semana_estadistica)) |> 
  arrange(anio, semana_estadistica)

# Crear objeto ts()
casos_neumonia_ts <- casos_neumonia |> 
  arrange(anio, semana_estadistica) |>
  pull(total_casos) |>
  ts(frequency = 52,
     start = c(min(casos_neumonia$anio), min(casos_neumonia$semana_estadistica)))

# Graficar la serie temporal
plot(casos_neumonia_ts, 
  main = "Casos de Neumonía por Semana",
  xlab = "Tiempo", 
  ylab = "Total de Casos", 
  col = "blue")

# Predicción
pred <- grnn_forecasting(casos_neumonia_ts, h = 10)
ro <- rolling_origin(pred, h = 20)
pred$prediction

# Indicadores predicción
ro$global_accu
ro$h_accu

plot(ro, h = 15)
plot(pred)
