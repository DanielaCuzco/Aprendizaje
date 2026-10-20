library(fpp3)
library(readr)
library(dplyr)


datos <- read_csv("C:/Users/DELL/Documents/data/data.csv") |>
  as_tsibble(key = c(Sub_nivel, Variable, Categoria), index = Anio)

# Gráfico 1
#En este caso se hizo este gráfico para mostrar la evolución del promedio en Matemática entre los años 2021 y 2025.
#Dado que la base de datos tene varias desagregaciones de los estudiantes (por Región, Sexo, Área y Sostenimiento). 
#Al graficarla tendría muchas líneas que no me dejaría ver algún patrón 
#Por lo que  apliqué un filtro Variable == "Nacional". 
#Esta variable contiene el consolidado general del país para cada subnivel educativo. 


datos |>
  filter(Variable == "Nacional") |>
  autoplot(P_mat) +
  geom_line(linewidth = 1) + # Líneas más gruesas
  geom_point(size = 2) +     # Puntos en los años
  theme_gray() +
  labs(
    title = "Promedio en Matemática por Subnivel",
    subtitle = "Filtro: Variable Nacional (2021-2025)",
    x = "Año",
    y = "Promedio (P_mat)",
    color = "Subnivel"
  ) +
  scale_x_continuous(breaks = 2021:2025)


#Gráfico sin filtrar la variable
datos |>
  # Solo filtramos por Sub_nivel para que no sean 4 gráficos distintos, sino uno solo
  autoplot(P_mat, alpha = 0.4) + # alpha = 0.4 hace que las líneas sean semitransparentes
  theme_gray() +
  labs(
    title = "Todas las categorías",
    subtitle = "Sin filtro (Mezcla de Nacional, Región, Sexo, Área y Sostenimiento)",
    x = "Año",
    y = "Promedio (P_mat)",
    color = "Serie"
  ) +
  theme(legend.position = "none") 


