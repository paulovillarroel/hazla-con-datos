library(tidyverse)
library(Microsoft365R)

# https://github.com/Azure/Microsoft365R

# Connect to OneDrive
od <- get_personal_onedrive()

# List files
od$list_items("Curso Ciencia de Datos")

# Get data
od$download_file(
  src = "Curso Ciencia de Datos/bd-onedrive.xlsx", 
  dest = "project-05/onedrive-db/src/bd-onedrive.xlsx",
  overwrite = TRUE
)

data_od <- readxl::read_excel("project-05/onedrive-db/src/bd-onedrive.xlsx")

data_od <- data_od |> 
  janitor::clean_names()

cie10 <- readxl::read_excel("project-05/onedrive-db/src/cie10.xlsx") |> 
  janitor::clean_names()

medicos <- readxl::read_excel("project-05/onedrive-db/src/medicos.xlsx") |> 
  janitor::clean_names()

# Join 
data_unida <- data_od |> 
  left_join(cie10, by = c("diagnostico_cie_10" = "codigo_cie_10")) |> 
  left_join(medicos, by = c("medico_asignado" = "medico")) |> 
  mutate(estancia = as.numeric(case_when(
    is.na(fecha_alta) ~ today() - ymd(fecha_ingreso),
    TRUE ~ fecha_alta - fecha_ingreso),
    hospitalizado = ifelse(is.na(fecha_alta), "SI", "NO"))
  )

# Save 
# To OneDrive
od$upload_file(
  src = "project-05/onedrive-db/output/data_unida.xlsx", 
  dest = "Curso Ciencia de Datos/data_unida.xlsx"
)

# Save to local file
writexl::write_xlsx(data_unida, "project-05/onedrive-db/output/data_unida.xlsx")
