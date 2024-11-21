library(tidyverse)
library(readxl)

## Desde un CSV

# library(readr) 
# Es imporante fijarse en el tipo de separador
# Cheat sheet Readr https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf
# No confundir read_csv con read.csv / Son de librerías distintas. read_csv es de readr()


read_csv(file.choose()) # Solo para casos muy especiales

covid_cases <- read_csv("example-data/casos_covid.csv") # Atencion a la codificación
covid_cases <- read_csv("example-data/casos_covid.csv", locale = locale(encoding = "latin1")) # UTF-8


# A veces, el CSV no tiene encabezados (headers)
covid_cases2 <- read_csv("example-data/casos_covid_sin_header.csv", locale = locale(encoding = "latin1"))

read_csv("example-data/casos_covid_sin_header.csv", 
         locale = locale(encoding = "latin1"), 
         col_names = FALSE) # Agrega nombres genéricos de columnas


read_csv("example-data/casos_covid_sin_header.csv", 
         locale = locale(encoding = "latin1"), 
         col_names = c( # Agregar nombres de columnas
           "Region",
           "Cod_region",
           "Comuna",
           "Cod_comuna",
           "Pobl",
           "Fecha",
           "Casos_conf")
         )

# Install.packages("data.table")
library(data.table)

data.table::fread("example-data/casos_covid.csv", encoding = "Latin-1") |> 
  View()

# install.packages("rio")
library(rio)

rio::import("example-data/casos_covid.csv", encoding = "Latin-1") |> 
  View()


## Desde Excel
library(readxl)

# Truco poco estético, pero útil. Copiar desde Excel (solo en casos muy puntuales)
df <- read.table(file = "clipboard", sep = "\t", header = TRUE)

# También se puede copiar desde una web
# https://gco.iarc.fr/today/en/dataviz/tables?types=1&mode=cancer&group_populations=1&multiple_populations=1

cancer <- read.table(file = "clipboard", sep = "\t", header = TRUE)

cancer |> 
  janitor::clean_names() |> 
  mutate(number = as.numeric(number)) # Ojo con la transformación de variables

cancer |> 
  janitor::clean_names() |> 
  mutate(number = str_remove_all(number, " "),
         number = as.numeric(number))

# Usando la ruta del archivo. Puedes ayudarte de la función getwd()
getwd()
read_xlsx("C:/Users/pvill/OneDrive/Proyectos-R/hazla-con-datos/gen2/example-data/poblacion_comunas.xlsx") |> 
  View()

population <- read_xlsx("example-data/poblacion_comunas.xlsx")

population <- read_excel("example-data/poblacion_comunas.xlsx")

rio::import("example-data/poblacion_comunas.xlsx")


# Muchas veces se incluyen títulos o logos en los excel
population2 <- read_excel("example-data/poblacion_comunas2.xlsx", skip = 6)

# Cuidado con celdas con NA´s
tail(population2, 2) # Filas con NA´s
head(population2, -2) |> View()

population2[1:362, 1:3]

population2 |>
  janitor::remove_empty("cols")

population2 |>
  head(-2) |> 
  janitor::remove_empty("cols")

population3 <- read_excel("example-data/poblacion_comunas2.xlsx", range = "D7:F369")

# Uso de ruta desde variable
df1 <- read_excel("example-data/indice_movilidad.xlsx", sheet = 2)

path <- "example-data/indice_movilidad.xlsx"
df2 <- read_excel(path, sheet = 3)

# Para importar varias hojas de Excel de forma simultanea
# Usaremos la función set_names() y map() de la librería PURRR (tidyverse)

path |> 
  excel_sheets()

mobility_index <- path |> 
  excel_sheets() |> 
  set_names() |> 
  map_df(read_excel, # Itera sobre cada hoja del Excel
         path = path,
         .id = "IM") # Esto agrega una nueva columna con el nombre de las hojas

# Abrir varios archivos

# Usaremos la función list.files() para listar los archivos de un directorio
# Luego, con la función map_df() de la librería purrr, importaremos los archivos
# La función map_df() es una versión de map() que devuelve un data frame

path <- "example-data/"

path |> 
  list.files(full.names = TRUE, pattern = "movilidad_")

all_files <- path |>
  list.files(full.names = TRUE, pattern = "movilidad_") |> 
  map_df(read_excel, .id = "IM")


# Leer multiples Excel con multiples hojas

all_files2 <- path |>
  list.files(full.names = TRUE, pattern = "movilidad") |> 
  set_names() |>
  map_df(~ .x |> 
          excel_sheets() |> 
          set_names() |> 
          map_df(read_excel, path = .x, .id = "Archivo"))


## Desde un repositorio en internet

public_consultation <- read_csv("https://raw.githubusercontent.com/MinCiencia/Politicas/main/Politica_Inteligencia_Artificial/Consulta_Publica/consulta_publica.csv")

eno <- read_csv2("https://datos.gob.cl/dataset/76aa7b9b-8a3b-404a-853d-0ecff159876d/resource/b6081caf-39af-4f3f-86de-d6e00739bdc4/download/20240304_base_eno_final.csv")

rem20 <- read_csv2("https://datos.gob.cl/dataset/a756323f-85ba-4759-87dc-f5d5e63868cc/resource/657cc933-eac8-4bfc-b004-c4d6dcd988a8/download/indicadores_rem20_20240425.csv")


# Desde datos espaciales (mapas)
# install.packages("sf")
# install.packages("chilemapas")
library(sf)
library(chilemapas)

comunas_rm <- mapa_comunas |> 
  filter(codigo_region == 13)

rm <- st_as_sf(comunas_rm) # Con esta función se puede trabajar como si fuera un data frame

ggplot() + 
  geom_sf(data = rm, aes(fill = codigo_comuna), show.legend = FALSE) +
  theme_void() +
  theme(legend.position = "none")

