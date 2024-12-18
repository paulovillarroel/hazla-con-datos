library(tidyverse)
library(RapidFuzz)

# Documentación https://github.com/StrategicProjects/RapidFuzz

query <- "cáncer"
choices <- c("pelota", "cancerígeno", "cncer")
score_cutoff <- 0.0

extract_matches(query, choices, score_cutoff, scorer = "PartialRatio")
