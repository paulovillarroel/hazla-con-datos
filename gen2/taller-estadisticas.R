library(tidyverse)
library(arrow)

data <- arrow::read_parquet("https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet")

data <- data |> 
  janitor::clean_names()

casos_neumonia_anio <- data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |>
  summarise(total_casos = sum(num_total), 
            .by = anio)

casos_neumonia_anio |> 
  ggplot(aes(anio, total_casos)) +
  geom_line()


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |> 
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |>
  summarise(total_casos = sum(casos),
            .by = c(edad_grupo, semana_estadistica)) |> 
  filter(edad_grupo != "num_total") |> 
  ggplot(aes(semana_estadistica, total_casos, color = edad_grupo)) +
  geom_line() +
  scale_color_brewer(palette = "Set1")


data |> 
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


data |> 
  filter(tipo_establecimiento == "Hospital",
          causa == "Neumonía (J12-J18)",
          anio == 2024) |> 
  summarise(casos_65 = sum(num65o_mas), .by = semana_estadistica) |> 
  ggplot(aes(semana_estadistica, casos_65)) +
  geom_line()


data |> 
  filter(tipo_establecimiento == "Hospital",
          causa == "Neumonía (J12-J18)",
          anio == 2024) |> 
  summarise(casos_65 = sum(num65o_mas), .by = c(establecimiento_glosa, semana_estadistica)) |>
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
