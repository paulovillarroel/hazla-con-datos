# install.packages("palmerpenguins")
# install.packages("tidyverse")

library(palmerpenguins)
library(tidyverse)

# Documentation https://dplyr.tidyverse.org/

# data(package = 'palmerpenguins')

penguins_df <- palmerpenguins::penguins
View(penguins_df)

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


# Data manipulation

penguins_mod <- penguins_df |> 
  select(species, sex)

glimpse(penguins_mod)


penguins_mod <- penguins_df |> 
  filter(sex == "female",
         bill_length_mm <= 40)

glimpse(penguins_mod)


penguins_mod <- penguins_df |> 
  arrange(bill_length_mm, desc(year))

glimpse(penguins_mod)


penguins_mod <- penguins_df |> 
  mutate(body_mass_kg = body_mass_g / 1000)

glimpse(penguins_mod)


penguins_mod <- penguins_df |> 
  group_by(sex) |> 
  summarize(mean_mass_g = mean(body_mass_g, na.rm = TRUE))

glimpse(penguins_mod)


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
