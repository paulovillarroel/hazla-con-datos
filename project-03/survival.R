library(tidyverse)

# https://www.fonasa.cl/sites/fonasa/datos-abiertos/bases-grd

# Unzip file
unzip("project-03/raw-data/GRD_PUBLICO_EXTERNO_2022.zip", exdir = "project-03/raw-data/")

# Save file into a variable
grd_2022 <- read_delim("project-03/raw-data/GRD_PUBLICO_EXTERNO_2022.txt",
                       delim = "|",
                       locale = locale(encoding = "UTF-16LE")
)

# Delete large files
file.remove("project-03/raw-data/GRD_PUBLICO_EXTERNO_2022.zip")
file.remove("project-03/raw-data/GRD_PUBLICO_EXTERNO_2022.txt")

# saveRDS(grd_2022, "project-03/intermediate-data/grd_2022.rds")
# readRDS("project-03/intermediate-data/grd_2022.rds")

# Read ICE-10 codes
icd_10 <- openxlsx::read.xlsx("project-03/raw-data/CIE-10.xlsx") |>
  janitor::clean_names()

# Join with hospitals codes

hospitals <- openxlsx::read.xlsx("project-03/raw-data/Establecimientos_DEIS.xlsx", startRow = 2) |>
  janitor::clean_names()


# Join with ICD-10 & hospitals codes
data_extended <- grd_2022 |>
  janitor::clean_names() |>
  mutate(cod_hospital = as.character(cod_hospital)) |>
  left_join(icd_10, by = c("diagnostico1" = "codigo")) |> 
  left_join(hospitals, by = c("cod_hospital" = "codigo_vigente"))

# Discharge malignant neoplasm cases

icd10_malignant_neoplasms <- c(
  "c0[0-9].*?|c0[0-9].*?",
  "c1[0-9].*?|c1[0-9].*?",
  "c2[0-9].*?|c2[0-9].*?",
  "c3[0-9].*?|c3[0-9].*?",
  "c4[0-9].*?|c4[0-9].*?",
  "c5[0-9].*?|c5[0-9].*?",
  "c6[0-9].*?|c6[0-9].*?",
  "c7[0-9].*?|c7[0-9].*?",
  "c8[0-9].*?|c8[0-9].*?",
  "c9[0-9].*?|c9[0-9].*?"
)

malignant_neoplasms <- data_extended |>
  filter(
    tipo_actividad == "HOSPITALIZACIÓN",
    str_detect(str_to_lower(categoria), paste(icd10_malignant_neoplasms, collapse = "|"))
  ) |>
  mutate(estancia = as.Date(fechaalta) - as.Date(fecha_ingreso)) |>
  select(cod_hospital, nombre_oficial, nivel_de_complejidad, nombre_dependencia_jerarquica_seremi_servicio_de_salud,
         sexo, tipo_ingreso, diagnostico1, descripcion, categoria, ir_29301_severidad, usospabellon,
         ir_29301_peso ,estancia, tipoalta) |> 
  rename(
    hospital = nombre_oficial,
    diagnostico = diagnostico1,
    descripcion = descripcion,
    categoria = categoria,
    servicio_salud = nombre_dependencia_jerarquica_seremi_servicio_de_salud,
    peso_grd = ir_29301_peso,
    severidad = ir_29301_severidad
  ) |> 
  mutate(servicio_salud = str_remove(servicio_salud, "Servicio de Salud "))


malignant_neoplasms |> 
  group_by(categoria) |> 
  summarise(total = n(), 
            p25 = round(quantile(estancia, 0.25), 1),
            mediana = round(median(estancia), 1),
            p75 = round(quantile(estancia, 0.75), 1)) |> View()

malignant_neoplasms |> 
  group_by(tipoalta) |> 
  summarise(total = n(), 
            p25 = round(quantile(estancia, 0.25), 1),
            mediana = round(median(estancia), 1),
            p75 = round(quantile(estancia, 0.75), 1)) |> View()


malignant_neoplasms_deceased <- malignant_neoplasms |>
  mutate(status = ifelse(tipoalta == "FALLECIDO", 1, 0),
         estancia = as.numeric(estancia),
         tipo_ingreso = as.factor(tipo_ingreso),
         severidad = as.factor(severidad),
         sexo = as.factor(sexo),
         servicio_salud = as.factor(servicio_salud),
         usospabellon = as.factor(usospabellon),
         peso_grd = as.numeric(str_replace(peso_grd, ",", "."))) |> 
  na.omit()

# Generate intervals for the DRG weight

breaks <- c(0, 1, 3, 5, 7, 9, 10, Inf)
labels <- c("0-1", "1-3", "3-5", "5-7", "7-9", "9-10", "10+")

malignant_neoplasms_deceased$peso_grd_intervalo <- cut(malignant_neoplasms_deceased$peso_grd, 
                                                       breaks = breaks, 
                                                       labels = labels, 
                                                       include.lowest = TRUE, 
                                                       right = FALSE)




# Survival analysis for malignant neoplasms using Kaplan-Meier
library(survival)
library(survminer)

# Survival curves for all malignant neoplasms
fit <- survfit(Surv(estancia, status) ~ 1, data = malignant_neoplasms_deceased)

summary(fit)
broom::tidy(fit) |> View()

# Basic survival curves
ggsurvplot(fit, data = malignant_neoplasms_deceased, surv.median.line = "hv")

# Survival curves by sex
fit2 <- survfit(Surv(estancia, status) ~ sexo, data = malignant_neoplasms_deceased)

broom::tidy(fit2) |> View()
summary(fit2, times = c(0, 10, 30, 45, 100))

ggsurvplot(fit2, data = malignant_neoplasms_deceased, surv.median.line = "hv",
           pval = TRUE, pval.size = 4)

# Survival curves severity
fit3 <- survfit(Surv(estancia, status) ~ severidad, data = malignant_neoplasms_deceased)

broom::tidy(fit3) |> View()
summary(fit3, times = c(0, 10, 30, 45, 100))

ggsurvplot(fit3, data = malignant_neoplasms_deceased, surv.median.line = "hv",
           pval = TRUE, pval.size = 4,
           title = "Curvas de supervivencia para pacientes oncológicos hospitalizados por severidad",
           subtitle = "Modelo Kaplan-Meier",
           xlab = "Días",
           ylab = "Probabilidad de supervivencia",
           legend.title = "Severidad",
           legend.labs = c("Severidad 1", "Severidad 2", "Severidad 3"))

# Survival curves by DRG weight
fit4 <- survfit(Surv(estancia, status) ~ peso_grd_intervalo, data = malignant_neoplasms_deceased)

broom::tidy(fit4) |> View()

summary(fit4, times = c(0, 10, 30, 45, 100))

ggsurvplot(fit4, data = malignant_neoplasms_deceased, surv.median.line = "hv",
           pval = TRUE, pval.size = 4,
           title = "Curvas de supervivencia para pacientes oncológicos hospitalizados por peso GRD",
           subtitle = "Modelo Kaplan-Meier",
           xlab = "Días",
           ylab = "Probabilidad de supervivencia",
           legend.title = "Peso GRD",
           legend.labs = c("0-1", "1-3", "3-5", "5-7", "7-9", "10+"))

# Survival curves for malignant neoplasms with severity 3

severety3 <- malignant_neoplasms_deceased |>
  filter(severidad == 3)

fit5 <- survfit(Surv(estancia, status) ~ peso_grd_intervalo, data = severety3)

ggsurvplot(fit5, data = severety3, surv.median.line = "hv",
           pval = TRUE, pval.size = 4)

# Survival curves for malignant neoplasms with severity 1

severety1 <- malignant_neoplasms_deceased |>
  filter(severidad == 1)

fit6 <- survfit(Surv(estancia, status) ~ peso_grd_intervalo, data = severety1)

ggsurvplot(fit6, data = severety1, surv.median.line = "hv",
           pval = TRUE, pval.size = 4)

# Survival curves for malignant neoplasms by service

fit7 <- survfit(Surv(estancia, status) ~ servicio_salud, data = malignant_neoplasms_deceased)

ggsurvplot(fit7, data = malignant_neoplasms_deceased, surv.median.line = "hv",
           pval = TRUE, pval.size = 4)

# Select some services
services <- c("Araucanía Norte", "Biobío", "Los Rios", "Metropolitano Oriente", "Metropolitano Sur Oriente")

malignant_neoplasms_deceased_selected <- malignant_neoplasms_deceased |>
  filter(servicio_salud %in% services)

fit8 <- survfit(Surv(estancia, status) ~ servicio_salud, data = malignant_neoplasms_deceased_selected)

ggsurvplot(fit8, data = malignant_neoplasms_deceased_selected, surv.median.line = "hv",
           pval = TRUE, pval.size = 4)

# Log-rank test for the different groups
survdiff(Surv(estancia, status) ~ severidad, data = malignant_neoplasms_deceased)
survdiff(Surv(estancia, status) ~ sexo, data = malignant_neoplasms_deceased)
survdiff(Surv(estancia, status) ~ usospabellon, data = malignant_neoplasms_deceased)


# PH for the different groups
cox_model <- coxph(Surv(estancia, status) ~ severidad, data = malignant_neoplasms_deceased)
cox_model_services <- coxph(Surv(estancia, status) ~ servicio_salud, data = malignant_neoplasms_deceased)
cox_model_surgeons <- coxph(Surv(estancia, status) ~ usospabellon, data = malignant_neoplasms_deceased)
coxph(Surv(estancia, status) ~ usospabellon + severidad, data = malignant_neoplasms_deceased)

# Relevel the reference category
malignant_neoplasms_deceased$servicio_salud <- relevel(malignant_neoplasms_deceased$servicio_salud, ref = "Metropolitano Sur Oriente")

cox_model_services <- coxph(Surv(estancia, status) ~ servicio_salud, data = malignant_neoplasms_deceased)


# Plot proportional hazards
ggforest(cox_model, data = malignant_neoplasms_deceased)
ggforest(cox_model_services, data = malignant_neoplasms_deceased)
ggforest(cox_model_surgeons, data = malignant_neoplasms_deceased)
