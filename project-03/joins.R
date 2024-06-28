library(tidyverse)

# Create a tibble

df1 <- tibble(
  id = c(1, 2, 3, 4),
  name = c("John", "Maria", "Peter", "Lucy")
)

df2 <- tibble(
  id = c(1, 2, 3, 5),
  age = c(22, 33, 44, 55)
)

df3 <- tibble(
  city = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia"),
  id = c(1, 2, 3, 4, 5, 6)
)

df4 <- tibble(
  person = c(1, 2, 3, 4, 5),
  profession = c("Engineer", "Doctor", "Teacher", "Nurse", "Pilot"),
)

df5 <- tibble(
  id = c(1, 2, 3, 4, 5),
  favorite_color = c("Blue", "Red", "Green", "Yellow", "Black"),
  pet = c("Dog", "Cat", "Bird", "Fish", "Rabbit"),
  gender = c("M", "F", "M", "F", "M")
)


# Left join
df1 |>
  left_join(df2, by = "id")

# Right join
df1 |>
  right_join(df2, by = "id")

# Inner join
df1 |>
  inner_join(df2, by = "id")

# Full join
df1 |>
  full_join(df2, by = "id")

# Anti join
df1 |>
  anti_join(df2, by = "id")

# Semi join
df1 |>
  semi_join(df2, by = "id")

# Join by different columns
df1 |>
  left_join(df4, by = c("id" = "person"))

df1 |>
  full_join(df4, by = c("id" = "person"))

# Cross join
df1 |>
  cross_join(df2, suffix = c("_1", "_2"))

# Nested join
out <- df1 |> 
  nest_join(df5)

out$df5

names(out$df5) <- out$name

out$df5$Maria

# Multiple joins
df1 |>
  left_join(df2, by = "id") |>
  left_join(df3, by = "id") |> 
  left_join(df4, by = c("id" = "person"))

df1 |> 
  inner_join(df2, by = "id") |>
  right_join(df3, by = "id")



