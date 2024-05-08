# Instalar las librerías
# install.packages("palmerpenguins")
# install.packages("tidyverse")

## Parte 1 ----

# Cargar las librerías
library(palmerpenguins)
library(tidyverse)

# Documentation https://dplyr.tidyverse.org/

# Crear nuestro objeto de pinguinos

# data(package = 'palmerpenguins')

penguins_df <- palmerpenguins::penguins
View(penguins_df)

# Analizar la estructura de los datos
glimpse(penguins_df)
skimr::skim(penguins_df)


# Data structures

# Vectors
penguins_vector <- penguins_df |> 
  pull(species)

penguins_vector2 <- penguins_df$species

# Tibble

penguins_tibble <- penguins_df |> 
  as_tibble()

# Data frame

penguins_df <- penguins_df |> 
  as.data.frame()

# Lists

penguins_list <- penguins_df |> 
  as.list()



## Parte 2 ----

# library(dplyr)
# Data manipulation with dplyr

# Select

penguins_mod <- penguins_df |> 
  select(species, island, sex) # Select columns

penguins_mod2 <- penguins_df |>
  select(-species, -sex) # Remove columns

penguins_mod3 <- penguins_df |>
  select(-c(species, sex)) # Remove columns using a vector


columns_to_select <- c("species", "island") # Create a vector with the columns to select

penguins_mod4 <- penguins_df |> 
  select(all_of(columns_to_select)) # Select using a vector

penguins_mod5 <- penguins_df |>
  select(any_of(columns_to_select))

glimpse(penguins_mod)

penguins_mod6 <- penguins_df |>
  select(1, 7, 2) # select by position

penguins_df |> 
  head() # first rows

penguins_df |> 
  tail(1) # last row


# Filter

penguins_mod <- penguins_df |> 
  filter(sex == "female",
         bill_length_mm <= 40)

penguins_mod <- penguins_df |> 
  filter(sex == "female" |
         bill_length_mm <= 40)


sex_to_filter <- c("female", "male")

penguins_df |>
  filter(sex == sex_to_filter)

penguins_na <- penguins_df |> 
  filter(is.na(sex))

penguins_wo_na <- penguins_df |> 
  filter(!is.na(sex))

penguins_df |> 
  na.omit() |> View()


glimpse(penguins_mod)


# Arrange

penguins_mod <- penguins_df |> 
  arrange(bill_length_mm, desc(year))

glimpse(penguins_mod)


# Mutate

penguins_df_na <- penguins_df |> 
  mutate(sex_na = is.na(sex))

penguins_mod <- penguins_df |> 
  mutate(body_mass_kg = round(body_mass_g / 1000, 1))

penguins_large <- penguins_df |> 
  mutate(large = ifelse(flipper_length_mm > 200, "Large", "Normal"))

penguins_large <- penguins_large |>
  mutate(large = ifelse(large == "Large", "L", "N"))

penguins_large2 <- penguins_df |>
  mutate(large = ifelse(flipper_length_mm > 200, "Large", "Normal"),
         large_short = ifelse(large == "Large", "L", "N"))


glimpse(penguins_mod)


# Group by

penguins_mod <- penguins_df |> 
  group_by(sex) |> 
  summarize(mean_mass_g = mean(body_mass_g, na.rm = TRUE))

glimpse(penguins_mod)


# All together

penguins_mod <- penguins_df |> 
  select(sex, bill_length_mm, body_mass_g) |> 
  filter(sex == "female",
         bill_length_mm > 30) |> 
  group_by(sex) |> 
  summarize(mean_mass_g = mean(body_mass_g, na.rm = TRUE))

glimpse(penguins_mod)




gentoo <- penguins_df |> 
  filter(species == "Gentoo") |> 
  select(species, bill_length_mm, sex)

mean_length <- mean(gentoo$bill_length_mm, na.rm = TRUE)
sd_length <- sd(gentoo$bill_length_mm, na.rm = TRUE)

gentoo <- gentoo |> 
  mutate(long_short = case_when(bill_length_mm > mean_length + 2*sd_length ~ "long", 
                                bill_length_mm < mean_length - 2*sd_length ~ "short",
                                TRUE ~ "ordinary"))

gentoo |> 
  janitor::tabyl(long_short) |> 
  janitor::adorn_pct_formatting(digits = 2)
