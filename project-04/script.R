library(tidyverse)
library(duckdb)
library(microbenchmark)

# Download the data

url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"
download.file(url, "project-04/raw-data/at_urg_respiratorio_semanal.parquet", mode = "wb")
data <- arrow::read_parquet("project-04/raw-data/at_urg_respiratorio_semanal.parquet")

# Connect to the database

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbWriteTable(con, "data_db", data)

# con <- dbConnect(duckdb::duckdb(), "data_db.duckdb")
# dbWriteTable(con, "data_table", data)

# Define the functions to compare

dplyr_function <- function() {
  data |>
    filter(Causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |> 
    group_by(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa) |> 
    summarise(
      p90 = quantile(NumTotal, 0.90),
      p10 = quantile(NumTotal, 0.10),
      .groups = 'drop') |> 
    mutate(rel_p90_p10 = if_else(p10 == 0, NA_real_, p90 / p10)) |> 
    arrange(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa)
}

db_parquet_function <- function() {
  tbl(con, "read_parquet('project-04/raw-data/at_urg_respiratorio_semanal.parquet')") |>
    filter(Causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |> 
    group_by(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa) |> 
    summarise(
      p90 = quantile(NumTotal, 0.90),
      p10 = quantile(NumTotal, 0.10),
      .groups = 'drop') |> 
    mutate(rel_p90_p10 = if_else(p10 == 0, NA_real_, p90 / p10)) |> 
    arrange(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa) |> 
    collect()
}

db_table_function <- function() {
  tbl(con, "data_db") |> 
    filter(Causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |> 
    group_by(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa) |> 
    summarise(
      p90 = quantile(NumTotal, 0.90),
      p10 = quantile(NumTotal, 0.10),
      .groups = 'drop') |> 
    mutate(rel_p90_p10 = if_else(p10 == 0, NA_real_, p90 / p10)) |> 
    arrange(SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa) |> 
    collect()
}

sql_function <- function() {
  dbGetQuery(con, 
             "
  WITH Percentiles AS (
      SELECT
          SemanaEstadistica,
          ServicioSaludGlosa,
          ComunaGlosa,
          PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY NumTotal) AS p90,
          PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY NumTotal) AS p10
      FROM data_db
      WHERE Causa = 'TOTAL CAUSAS SISTEMA RESPIRATORIO'
      GROUP BY SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa
  )
  SELECT
      SemanaEstadistica,
      ServicioSaludGlosa,
      ComunaGlosa,
      p90,
      p10,
      CASE WHEN p10 = 0 THEN NULL ELSE p90 / p10 END AS rel_p90_p10
  FROM Percentiles
  ORDER BY SemanaEstadistica, ServicioSaludGlosa, ComunaGlosa;
  ")
}

# Compare the performance of the different methods
benchmark_results <- microbenchmark(
  dplyr = dplyr_function(),
  db_parquet = db_parquet_function(),
  db_table = db_table_function(),
  sql = sql_function(),
  times = 10
)

print(benchmark_results)

dbDisconnect(con)
