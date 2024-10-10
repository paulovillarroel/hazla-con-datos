## Tipos y estructura de Datos
 
# Ejemplos de Coerción
# Coerción automática
valores <- c(1, "2", TRUE) # R convierte los números y lógicos a caracteres
print(valores)
print(paste("Tipo de datos:", class(valores)))

# Coerción explícita
numero <- 42.5
print(paste("Número original:", numero))
print(paste("Como entero:", as.integer(numero))) # Convierte a entero

factor_nombres <- factor(c("rojo", "verde", "azul"))
print(factor_nombres)
print(paste("Como carácter:", as.character(factor_nombres))) # Convierte factor a carácter
print(paste("Como número:", as.numeric(factor_nombres))) # Convierte factor a número (niveles)

# Estructuras de Datos
# 1. Vectores
vector_logico <- c(TRUE, FALSE, TRUE)
print(vector_logico)

# 2. Matrices
matriz_ejemplo <- matrix(1:6, nrow = 2, byrow = TRUE)
print(matriz_ejemplo)

# 3. Data Frames
df_usuarios <- data.frame(
  usuario = c("Alice", "Bob", "Carlos"),
  edad = c(23, 35, 28),
  activo = c(TRUE, FALSE, TRUE)
)
print(df_usuarios)

# 4. Listas
lista_mixta <- list(
  nombre = "ChatGPT",
  puntuacion = 9.8,
  caracteristicas = c("rápido", "preciso", "versátil")
)
print(lista_mixta)

lista_mixta$nombre # Acceder a un dato interno
lista_mixta[1]

# Operadores
# Operadores Aritméticos
x <- 10
y <- 3
print(paste("Suma:", x + y))
print(paste("Resta:", x - y))
print(paste("Multiplicación:", x * y))
print(paste("División:", x / y))
print(paste("Módulo:", x %% y))  # Resto de la división
print(paste("Potencia:", x ^ y))

# Operadores Relacionales
print(paste("x es igual a y:", x == y))
print(paste("x es mayor que y:", x > y))
print(paste("x es menor o igual que y:", x <= y))

# Operadores Lógicos
a <- TRUE
b <- FALSE
print(paste("a AND b:", a & b))
print(paste("a OR b:", a | b))
print(paste("NOT a:", !a))

# Tidy Data con data frames
# Data frame original
df_tidy <- data.frame(
  id = 1:3,
  nombre = c("Ana", "Juan", "Lucía"),
  valor = c(5.2, 6.1, 7.3)
)
print(df_tidy)

# Manipulación de datos
# Instalara librería Tidyverse
# install.packages("tidyverse")

library(tidyverse) # Cargar librería

# Filtrar usuarios mayores de 30 años
usuarios_filtrados <- subset(df_usuarios, edad > 30)
print(usuarios_filtrados)

# Crear una nueva columna
df_usuarios$salario <- c(2000, 3000, 2500)
print(df_usuarios)

# Ordenar por edad
usuarios_ordenados <- df_usuarios[order(df_usuarios$edad), ]
print(usuarios_ordenados)
