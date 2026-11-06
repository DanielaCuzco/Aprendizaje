library(fpp3)
library(tidyverse)
library(readr)
library(here)
library(slider)

energia <- read_csv(here("data", "data_energia.csv"))

energia
class(energia)


energia <- energia |> 
  filter(!is.na(fecha_hora))

energia_ts <- energia |> 
  as_tsibble(index = fecha_hora)

#bVer que todo quedó bien
head(energia_ts)


#Descomposición multiplicativa 

#Filtremos solo para el año 2016, que se puedan visualizar mejor las cosas

energia_2016 <- energia_ts |>
  filter(year(fecha_hora)==2016) 

autoplot(energia_2016, demanda_real) + 
  labs(
    title = "Demanda eléctrica en España (2016)",
    y = "Demanda (MW)", 
    x = "Fecha" 
  )


#Este para ver de un día con 24 horas, usamos medias móviles de medias móviles

energ_med_m <- energia_2016 |>
  mutate(
    `24-MA` = slide_dbl(
      demanda_real, 
      mean,             
      .before = 11,
      .after = 12,
      .complete = TRUE  
    ),
    
    #Centramos la de 24 horas con una media de 2
    Tt = slide_dbl(
      `24-MA`, 
      mean, 
      .before = 1,   # Toma el valor actual y el anterior
      .after = 0,    
      .complete = TRUE
    )
    
  )

energ_med_m |> autoplot(demanda_real) +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1.2) +
  labs(
    x = "Fecha", 
    y = "Demanda (MW)", 
    title = "Demanda eléctrica y su media móvil de 24 horas"
  )

#Paso 2
#Recordemos que yt_pt es el resultado de la serie sin tendencia 
#precio_cierre = yt
datos <- energ_med_m  |>
  mutate(yt_pt= demanda_real / Tt)
autoplot(datos, yt_pt) + labs(title = "Serie sin tendencia (yt / Tt)")



#PASO 3
comp_estacional <- datos |>
  group_by(hour(fecha_hora)) |>
  mutate(st= mean(yt_pt, na.rm=TRUE))

autoplot(comp_estacional,st) + labs(x = "Fecha",
                                    y = "Efecto estacional (S_t)", title="Componente estacional (S_t) por hora del día")

#PASO 4 
restante <- comp_estacional |>
  mutate(Rt = demanda_real/ (Tt*st))

autoplot(restante, Rt) + labs(x= "Fecha", y = "Residuo (R_t)", title = "Componente Resto (Ruido) de la demanda elétrica")

