library(tidyverse)

eno <- read_csv2("https://datos.gob.cl/dataset/76aa7b9b-8a3b-404a-853d-0ecff159876d/resource/b6081caf-39af-4f3f-86de-d6e00739bdc4/download/20240304_base_eno_final.csv")


# Cuántas columnas y filas tiene el dataset?
dim(eno)
nrow(eno)
ncol(eno)

# ¿Cuáles son los nombres de las columnas?
colnames(eno)

# ¿Cuáles son los tipos de datos de cada columna?
str(eno)

# Elimina todas las columnas con valores vacios (NA´s) y sobreescribe el dataset original
eno <- eno |> 
  janitor::remove_empty("cols")

# ¿Cuál es el rango de años cubierto por el dataset (de las notificaciones)?
eno |>
  distinct(anho_notificacion) |> 
  arrange(anho_notificacion)

eno |> 
  summarise(min_year = min(anho_notificacion), 
            max_year = max(anho_notificacion)
          )

# Qué enfermedades están incluidas en el dataset?
unique(eno$ENO)

# ¿Cuántos casos de cada enfermedad se registran en el dataset?
eno |> 
  group_by(ENO) |> 
  summarise(total_casos = n()) |> 
  View()

# Para el año 2022, ¿Cuál es la región con más casos registrados de Coqueluche y cuántos casos son?
eno |> 
  filter(ENO == "Coqueluche",
         anho_notificacion == 2022) |>
  group_by(region) |>
  summarise(total_casos = n()) |>
  arrange(desc(total_casos)) |>
  head(1)

# Calcula el promedio de notificaciones por año para cada región
eno |> 
  group_by(region, anho_notificacion) |>
  summarise(promedio_notificaciones = mean(n())) |> 
  View()

# ¿Cuál es la región con más casos registrados en el dataset (de cualquier enfermedad) y cuántos casos son?
eno |>
  group_by(region) |>
  summarise(total_casos = n()) |> 
  arrange(desc(total_casos)) |>
  head(1)

# ¿Cuáles son las 3 enfermedades con más casos registrados por cada año?
eno |>
  group_by(anho_notificacion, ENO) |>
  summarise(total_casos = n()) |>
  arrange(anho_notificacion, desc(total_casos)) |>
  group_by(anho_notificacion) |>
  slice_max(order_by = total_casos, n = 3)

