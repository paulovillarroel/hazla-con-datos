library(tidyverse)
library(arrow)

data <- arrow::read_parquet("https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet")


# Parte 1 ----

flu_hospitals <- data |>
  filter(
    TipoUrgencia == "Urgencia Hospitalaria (UEH)",
    Causa == "Influenza (J09-J11)"
  ) |> 
  group_by(Anio, SemanaEstadistica) |> 
  summarise(casos = sum(NumTotal)) |>
  ungroup() |>
  janitor::clean_names()

flu_hospitals <- flu_hospitals |>
  filter(!(anio == 2024 & semana_estadistica == last(semana_estadistica)))


ggplot(data = flu_hospitals, aes(x = semana_estadistica, y = casos, color = factor(anio))) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(flu_hospitals$semana_estadistica), max(flu_hospitals$semana_estadistica), by = 5)) +
  scale_color_manual(values = c(rep("#ced4da", times = length(unique(flu_hospitals$anio)) - 1), "#ff006e")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Año 2024 resaltado",
    x = "Semana epidemiológica",
    y = "Casos",
    color = "Año",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8, face = "bold"))



# Parte 2 ----

flu_hospitals_per_region <- data |>
  filter(
    TipoUrgencia == "Urgencia Hospitalaria (UEH)",
    Causa == "Influenza (J09-J11)"
  ) |>
  group_by(RegionGlosa, Anio, SemanaEstadistica) |>
  summarise(casos = sum(NumTotal)) |>
  ungroup() |>
  janitor::clean_names()

flu_hospitals_per_region <- flu_hospitals_per_region |>
  filter(!(anio == 2024 & semana_estadistica == last(semana_estadistica)))


ggplot(data = flu_hospitals_per_region, aes(x = semana_estadistica, y = casos, color = factor(anio))) +
  geom_line(linewidth = 0.6) +
  scale_x_continuous(breaks = seq(min(flu_hospitals_per_region$semana_estadistica), max(flu_hospitals_per_region$semana_estadistica), by = 5)) +
  scale_color_manual(values = c(rep("#ced4da", times = length(unique(flu_hospitals_per_region$anio)) - 1), "#ff006e")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza, por regiones de Chile",
    subtitle = "Año 2024 resaltado",
    x = "Semana epidemiológica",
    y = "Casos",
    color = "Año",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8, face = "bold")) +
  facet_wrap(~ region_glosa, scales = "free_y")



# Parte 3 ----

library(ggtext)

unique_years <- unique(flu_hospitals$anio)

# Definir los colores para los años 2023 y 2024
colors <- rep("#ced4da", times = length(unique_years))
colors[unique_years == 2023] <- "#8338ec"
colors[unique_years == 2024] <- "#ff006e"

# Obtener el último punto de datos para el año 2024
max_2024 <- flu_hospitals |> 
  filter(anio == 2024) |> 
  slice(which.max(casos))

# Obtener el valor máximo para el 2023
max_2023 <- flu_hospitals |> 
  filter(anio == 2023) |> 
  slice(which.max(casos))


ggplot(data = flu_hospitals, aes(x = semana_estadistica, y = casos, color = factor(anio))) +
  geom_line(linewidth = 0.8) +
  geom_text(data = max_2024, aes(label = casos), vjust = 0, hjust = -0.2, size = 3.5) +
  geom_text(data = max_2023, aes(label = casos), vjust = 0, hjust = -0.2, size = 3.5) +
  geom_point(data = max_2024, aes(x = semana_estadistica, y = casos), size = 3, color = "#ff006e") +
  geom_point(data = max_2023, aes(x = semana_estadistica, y = casos), size = 3, color = "#8338ec") +
  scale_x_continuous(breaks = seq(min(flu_hospitals$semana_estadistica), max(flu_hospitals$semana_estadistica), by = 5)) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Se han resaltado los años <span style='color:#8338ec;'>2023</span> y <span style='color:#ff006e;'>2024</span>",
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 12, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8, face = "bold")
  )

