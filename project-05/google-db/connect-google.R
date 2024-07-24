library(tidyverse)
library(googlesheets4)

# https://googlesheets4.tidyverse.org/

# Connect to Google Drive
gs4_auth()

# Get data
data <- read_sheet("https://docs.google.com/spreadsheets/d/1aTBVMGLUG5K5gYJndrjn0j02YltGyRgNYvseCNc6EBc/edit?usp=sharing")

data <- data |> 
  janitor::clean_names()

cie10 <- readxl::read_excel("project-05/google-db/src/cie10.xlsx") |> 
  janitor::clean_names()

medicos <- readxl::read_excel("project-05/google-db/src/medicos.xlsx") |> 
  janitor::clean_names()

# Join 
data_unida <- data |> 
  left_join(cie10, by = c("diagnostico_cie_10" = "codigo_cie_10")) |> 
  left_join(medicos, by = c("medico_asignado" = "medico")) |> 
  mutate(estancia = as.numeric(case_when(
    is.na(fecha_alta) ~ today() - ymd(fecha_ingreso),
    TRUE ~ fecha_alta - fecha_ingreso),
    hospitalizado = ifelse(is.na(fecha_alta), "SI", "NO"))
  )

# Save
# To Google Sheet
sheet <- gs4_create("data_unida", sheets = list(data_unida = data_unida))

# Get link
gs4_get(sheet)$spreadsheet_url

# Save to local file
writexl::write_xlsx(data_unida, "project-05/google-db/output/data_unida.xlsx")

