library(tidyverse)
library(janitor)

# Datos Abiertos MINSAL Chile https://datos.gob.cl/organization/ministerio_de_salud

covid <- read_csv2("https://datos.gob.cl/dataset/8982a05a-91f7-422d-97bc-3eee08fde784/resource/8e5539b7-10b2-409b-ae5a-36dae4faf817/download/defunciones_covid19_2020_2024.csv") # Desde link

covid2 <- read_csv2("gen2/raw-data/defunciones_covid19_2020_2024.csv") # Desde archivo local

# install.packages("janitor")
covid <- covid |>
  janitor::clean_names() # Ajusta nombres columnas

colnames(covid)

help(distinct)

distinct(covid, ano)

covid |> 
  distinct(ano)

help(count)

covid |> 
  count(comuna) |> 
  View()

covid |> 
  filter(ano == 2024) |> 
  count(comuna) |> 
  arrange(desc(n)) |> 
  #top_n(3)
  slice_max(n, n = 3)

help(top_n)


covid |> 
  select(comuna, nombre_region)

columnas_omitir <- c("comuna", "nombre_region")

covid |> 
  select(-columnas_omitir)

covid |> 
  select(where(~ !all(is.na(.)))) # Seleccionar solo columnas con datos

covid3 <- covid |> 
  janitor::remove_empty("cols")

covid |> 
  janitor::remove_empty_cols() #deprecadas

help(tabyl)

lugar_def <- covid$lugar_defuncion

covid |> 
  filter(ano == 2020) |>
  tabyl(lugar_defuncion) |>
  adorn_pct_formatting() |> 
  adorn_totals("row")

covid |> 
  group_by(sexo_nombre) |> 
  summarise(n = n())

covid |> 
  group_by(lugar_defuncion, sexo_nombre) |> 
  summarise(n = n())

covid |> 
  group_by(ano) |>
  summarise(media = mean(edad_cant),
            mediana = median(edad_cant),
            min = min(edad_cant),
            max = max(edad_cant),
            n = n())

covid |> 
  filter(edad_cant == 1) |>
  group_by(comuna) |> 
  summarise(n = n())

p <- covid |> 
  group_by(ano) |> 
  summarise(n = n())

plot(p)

p |> 
  ggplot(aes(ano, n)) +
  geom_line(size = 2, color = "#f49cbb") +
  geom_point(size = 4, color = "#880d1e") +
  theme_minimal() +
  labs(title = "Mi primer gráfico con R y ggplot")

covid |> 
  mutate(fecha = zoo::as.yearmon(fecha_def)) |>
  group_by(nombre_region, fecha) |> 
  summarise(n = n()) |>
  ggplot(aes(fecha, n)) +
  geom_line(color = "#b5179e") +
  theme_minimal() +
  facet_wrap(~ nombre_region)

covid |> 
  mutate(menor_40 = edad_cant < 40) |> 
  filter(menor_40 == TRUE) |> 
  group_by(nombre_region, fecha_def) |> 
  summarise(n = n()) |>
  ggplot(aes(fecha_def, n)) +
  geom_point(color = "#b5179e") +
  theme_minimal() +
  facet_wrap(~ nombre_region)

# install.packages("zoo")
# https://www.rdocumentation.org/packages/zoo/versions/1.8-12/topics/yearmon
covid |> 
  mutate(fecha = zoo::as.yearmon(fecha_def),
         menor_40 = edad_cant < 40) |>
  filter(menor_40 == TRUE) |> 
  group_by(fecha, nombre_region) |> 
  summarise(n = n()) |>
  ggplot(aes(fecha, n)) +
  geom_line(color = "#b5179e") +
  theme_minimal()