library(tidyverse)

# Archivo 1
url1 <- "https://chc-oc-files.s3.amazonaws.com/sector/2023/Sem1/Salud.7z"
download.file(url1, "project-02/raw-data/Salud1.7z", mode = "wb")

file_list1 <- archive::archive("project-02/raw-data/Salud1.7z")

extracted_file_path1 <- archive::archive_extract("project-02/raw-data/Salud1.7z", file_list1$path[4], dir = tempdir())

direct_contact1 <- readr::read_csv2(file.path(tempdir(), file_list1$path[4]), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

# Archivo 2
url2 <- "https://chc-oc-files.s3.amazonaws.com/sector/2023/Sem2/Salud.7z"
download.file(url2, "project-02/raw-data/Salud2.7z", mode = "wb")

file_list2 <- archive::archive("project-02/raw-data/Salud2.7z")

extracted_file_path2 <- archive::archive_extract("project-02/raw-data/Salud2.7z", file_list2$path[4], dir = tempdir())

direct_contact2 <- readr::read_csv2(file.path(tempdir(), file_list2$path[4]), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

# Combinar ambos archivos
direct_contact <- bind_rows(direct_contact1, direct_contact2)


# Exploratory Data Analysis

direct_contact |> 
  glimpse()

# Missing values (NA)
library(naniar)

n_miss(direct_contact)

n_complete(direct_contact)

prop_miss(direct_contact)

prop_complete(direct_contact)

gg_miss_upset(direct_contact)

gg_miss_var(direct_contact)

miss_var_summary(direct_contact)

miss_case_summary(direct_contact) |> View()

direct_contact |> 
  filter(row_number() == 84031) |> View() #slice

direct_contact |> 
  filter(row_number() %in% c(84549, 84549, 87648, 87649, 89640)) |> View()

direct_contact |> 
  slice(84549, 84549, 87648, 87649, 89640) |> View()


# Data cleaning

direct_contact <- direct_contact |> 
  mutate(fecha_envio_oc = as.Date(fecha_envio_oc, format = "%d-%m-%Y"))

# direct_contact |> 
#   separate(fecha_envio_oc, into = c("fecha", "hora"), sep = " ") |>
#   mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"),
#          hora = lubridate::hms(hora)) |> View()
  

direct_contact |>
  group_by(mes = month(fecha_envio_oc)) |>
  summarise(total = n())


# Data visualization & analysis

direct_contact |> 
  summarise(total = sum(monto_neto_item_clp))

format(sum(direct_contact$monto_neto_item_clp), scientific = FALSE)

direct_contact |> 
  group_by(mes = month(fecha_envio_oc)) |>
  summarise(n = n(),
            total = sum(monto_neto_item_clp),
            prom_oc = total / n )

direct_contact |> 
  group_by(region_unidad_compra,
           mes = month(fecha_envio_oc)) |>
  summarise(n = n(),
            total = sum(monto_neto_item_clp),
            prom_oc = total / n ) |> View()


direct_contact |> 
  ggplot(aes(x = fecha_envio_oc)) +
  geom_histogram(stat = "count") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")


direct_contact |> 
  filter(estado_oc != "Solicitud de Cancelacion") |> 
  summarise(total = sum(monto_neto_item_clp), .by = rubro_n1) |>
  arrange(desc(total)) |>
  top_n(20) |> 
  ggplot(aes(x = fct_reorder(rubro_n1, total), y = total)) +
  #ggplot(aes(rubro_n1, total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)


direct_contact |> 
  filter(estado_oc != "Solicitud de Cancelacion") |> 
  summarise(total = sum(monto_neto_item_clp), .by = proveedor) |>
  arrange(desc(total)) |>
  top_n(20) |> 
  ggplot(aes(x = fct_reorder(proveedor, total), y = total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)


direct_contact |> 
  filter(estado_oc != "Solicitud de Cancelacion") |> 
  summarise(total = sum(monto_neto_item_clp), .by = onu_producto) |>
  arrange(desc(total)) |>
  top_n(20) |> 
  ggplot(aes(x = fct_reorder(onu_producto, total), y = total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)


direct_contact |> 
  filter(estado_oc != "Solicitud de Cancelacion") |> 
  summarise(total = sum(monto_neto_item_clp), .by = institucion) |>
  arrange(desc(total)) |>
  top_n(20) |> 
  ggplot(aes(x = fct_reorder(institucion, total), y = total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)


direct_contact |> 
  filter(institucion == "SERVICIO DE SALUD OCCIDENTE HOSPITAL SAN JUAN DE DIOS",
         estado_oc != "Solicitud de Cancelacion") |> 
  mutate(es_medico = case_when(
    onu_producto == "Personal médico temporal" ~ "Médicos",
    TRUE ~ "Otros"
  )) |>
  group_by(es_medico) |>
  summarise(total = sum(monto_neto_item_clp)) |>
  ggplot(aes(x = es_medico, y = total)) +
  geom_col()

direct_contact |> 
  filter(institucion == "SERVICIO DE SALUD OCCIDENTE HOSPITAL SAN JUAN DE DIOS",
         estado_oc != "Solicitud de Cancelacion") |> 
  group_by(onu_producto) |>
  summarise(total = sum(monto_neto_item_clp)) |>
  arrange(desc(total)) |>
  top_n(20) |> 
  ggplot(aes(x = fct_reorder(onu_producto, total), y = total)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
  
top_suppliers <- direct_contact |>
  filter(onu_producto == "Servicios quirúrgicos",
         estado_oc != "Solicitud de Cancelacion") |> 
  group_by(mes = month(fecha_envio_oc), proveedor) |>
  summarise(total = sum(monto_neto_item_clp), .groups = 'drop') |>
  arrange(desc(total)) |> 
  group_by(mes) |>
  slice_max(total, n = 10)

top_suppliers |> 
  group_by(proveedor) |>
  summarise(n = n(),
            monto = sum(total)) |> 
  arrange(desc(n)) |> 
  View()
 
