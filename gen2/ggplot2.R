library(tidyverse)
library(janitor)

eno <- read_csv2("https://datos.gob.cl/dataset/76aa7b9b-8a3b-404a-853d-0ecff159876d/resource/b6081caf-39af-4f3f-86de-d6e00739bdc4/download/20240304_base_eno_final.csv")

eno <- eno |>
  clean_names() |> 
  janitor::remove_empty("cols")

# Barras
eno |> 
  group_by(anho_notificacion) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_bar(stat = "identity", color = "red", fill = "steelblue")

# Líneas
eno |> 
  group_by(anho_notificacion) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue")

# Puntos
eno |> 
  group_by(anho_notificacion) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_point(color = "steelblue", size = 3)

# Líneas y puntos
eno |> 
  group_by(anho_notificacion) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3)

# Ajustar años eje x
eno |> 
  group_by(anho_notificacion) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = seq(2007, 2022, by = 1)) + 
  # Agregar valores a cada punto
  geom_text(aes(label = n), vjust = -1, nudge_y = 500, size = 3)

# Separar por sexo
eno |> 
  filter(sexo != "***") |> 
  group_by(anho_notificacion, sexo) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = seq(2007, 2022, by = 1)) + 
  # Agregar valores a cada punto
  geom_text(aes(label = n), vjust = -1, size = 3, nudge_y = 500) +
  facet_wrap(~ sexo) +
  theme_bw() +
  theme(strip.background.x = element_rect(fill = "#9c94d5"))

# Agregar linea de tendencia
eno |> 
  filter(sexo != "***") |> 
  group_by(anho_notificacion, sexo) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "loess", color = "#f72585") + # Tendencia
  scale_x_continuous(breaks = seq(2007, 2022, by = 1)) + 
  geom_text(aes(label = n), vjust = -1, size = 3, nudge_y = 500) +
  facet_wrap(~ sexo) +
  theme_bw() +
  theme(strip.background.x = element_rect(fill = "#9c94d5"))

# Agregar título y subtítulo
eno |> 
  filter(sexo != "***") |> 
  group_by(anho_notificacion, sexo) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = anho_notificacion, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "#f72585") + # Tendencia
  scale_x_continuous(breaks = seq(2007, 2022, by = 1)) + 
  geom_text(aes(label = n), vjust = -1, size = 3, nudge_y = 500) +
  facet_wrap(~ sexo) +
  theme_bw() +
  theme(strip.background.x = element_rect(fill = "#9c94d5")) +
  labs(title = "Notificaciones de enfermedades neurológicas en Chile (2007-2022)",
       x = "Año de notificación",
       y = "N° de notificaciones",
       caption = "Fuente: Datos Abiertos DEIS")

eno |> 
  filter(eno == "Coqueluche", anho_notificacion == 2022) |>
  group_by(region) |>
  summarise(total_casos = n()) |>
  arrange(desc(total_casos)) |>
  head(1)
