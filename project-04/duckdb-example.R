library(tidyverse)
library(duckdb)

# Download the data

url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"
download.file(url, "project-04/raw-data/at_urg_respiratorio_semanal.parquet", mode = "wb")
data <- arrow::read_parquet("project-04/raw-data/at_urg_respiratorio_semanal.parquet")

# Connect to the database DuckDB

con <- dbConnect(duckdb::duckdb(), ":memory:")
duckdb_register(con, "data_duck", data)

dbListTables(con) # Check the tables in the database
dbListFields(con, "data_duck") # Check the fields in the table

# con <- dbConnect(duckdb::duckdb(), "project-04/data_db.duckdb")
# dbWriteTable(con, "data_duck", data)

# Using dplyr

estab_agrupados_top <- tbl(con, "data_duck") |>
  filter(Anio == 2024) |>
  group_by(EstablecimientoGlosa) |>
  summarise(total = sum(NumTotal, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  head(10) |> 
  collect()


# Using SQL

dbGetQuery(
  con,
  "SELECT establecimientoglosa, sum(numtotal) AS total
FROM data_duck
WHERE anio = 2024
GROUP BY establecimientoglosa
ORDER BY total DESC
LIMIT 10"
)


dbDisconnect(con)
