library(tidyverse)
library(arrow)

# Opción 1
url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

destfile <- tempfile(fileext = ".parquet") # Archivo temporal
download.file(url, destfile, mode = "wb") # Descarga el archivo

data <- read_parquet(destfile)


# Opción 2
# install.packages("httr")
library(httr)

download_parquet <- function(url, max_attempts = 5) {
  temp_file <- tempfile(fileext = ".parquet")
  
  for (i in 1:max_attempts) {
    message(sprintf("Intento %d de %d", i, max_attempts))
    
    tryCatch({
      response <- GET(url, write_disk(temp_file, overwrite = TRUE))
      
      if (status_code(response) == 200) {
        message("Descarga exitosa")
        return(read_parquet(temp_file))
      }
    }, error = function(e) {
      if (i == max_attempts) stop("Error en la descarga después de ", max_attempts, " intentos")
      Sys.sleep(2)
    })
  }
}

# Uso
url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

data <- download_parquet(url)


# Exploración de datos

data <- data |> 
  janitor::clean_names()

casos_neumonia_anio <- data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |>
  group_by(anio) |> 
  summarise(total_casos = sum(num_total)) |> 
  ungroup()
  # summarise(total_casos = sum(num_total), 
  #           .by = anio)

casos_neumonia_anio |> 
  ggplot(aes(anio, total_casos)) +
  geom_line()


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |> 
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |>
  summarise(total_casos = sum(casos),
            .by = c(anio, edad_grupo, semana_estadistica)) |> 
  filter(edad_grupo == "num5a14anios") |>
  ggplot(aes(semana_estadistica, total_casos, color = edad_grupo)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ anio)


data_longer <- data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |> 
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |> 
  filter(edad_grupo == "num65o_mas") |>
  summarise(total_casos = sum(casos), 
            mas_65 = sum(casos), 
            porc_65 = (mas_65 / total_casos) * 100,
            .by = c(region_glosa, semana_estadistica))

data_longer |> # rafactoring
  filter(edad_grupo == "num65o_mas") |>
  summarise(mas_65 = sum(casos),
            .by = c(region_glosa, semana_estadistica))


data |> 
  filter(tipo_establecimiento == "Hospital",
          causa == "Neumonía (J12-J18)",
          anio == 2024) |> 
  summarise(casos_65 = sum(num65o_mas), 
            .by = semana_estadistica) |> 
  ggplot(aes(semana_estadistica, casos_65)) +
  geom_line()


data |> 
  filter(tipo_establecimiento == "Hospital",
          causa == "Neumonía (J12-J18)",
          anio == 2024) |> 
  summarise(casos_65 = sum(num65o_mas), 
            .by = c(establecimiento_glosa, semana_estadistica)) |>
  ggplot(aes(casos_65)) +
  geom_density()


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |> 
summarise(promedio = mean(num65o_mas),
          mediana = median(num65o_mas),
          p80 = quantile(num65o_mas, probs = 0.8),
          .by = establecimiento_glosa) |> View()


data |> 
  filter(tipo_establecimiento == "Hospital",
          causa == "Neumonía (J12-J18)",
          anio == 2024,
          establecimiento_glosa == "Complejo Asistencial Dr. Víctor Ríos Ruiz (Los Ángeles)") |>
  ggplot(aes(num65o_mas)) +
  geom_density()
