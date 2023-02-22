###############Ejercicio: Mapas interactivo de Colombia por COVID-19#################
#############################################################################
######################Autor: Juan Pablo Cely#################################
###############################15-04-2020####################################
rm(list=ls())
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(viridis)
library(ggthemes)


#https://arcruz0.github.io/libroadp/mapas.html
directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
#https://vizualdatos.com/como-separar-columna-unificada-coordenadas-variables-longitud-latitud-con-r/
colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")

#-----------------Nota

#si no puedo encontrar las coordenadas, mer remito a readOGR() para capturar las coordenadas 


covid19 <- read_excel("~/Documents/Investigacion/covid19/covid2.xlsx")

colom19 <- colombia %>% 
  left_join(covid19)


i <- ggplot(colom19)+
  geom_sf(aes(fill=Contagios))+  xlab("") + ylab("") + 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(Dia)+
  labs(subtitle = "Dia: {current_frame}")+
  labs(title = "Evolución del Covid-19 en Colombia",
                                                caption = "Fuente: ISN")


animate(i, renderer = gifski_renderer("Contagios2.gif"))





#https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
#https://tereom.github.io/tutoriales/mapas_R_blog
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html   encontrar coordenadas 

#burble

colombia33 <- colombia %>% 
  left_join(covid19)

colombia33 <- cbind(colombia33, st_coordinates(st_centroid(colombia33$geometry)))

i <- ggplot(colombia33)+
  geom_sf(aes(fill=Contagios))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = Contagios),
             colour = 'red', alpha = .5) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(1, 25, 50, 100, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200))+ 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(Dia)+labs(size = 'Contagios') +
  labs(subtitle = "Dia: {current_frame}")+
  labs(title = "Evolución del Covid-19 en Colombia",
       caption = "Fuente: ISN")

animate(i, renderer = gifski_renderer("Contagiosprueba.gif"))

colombia33$






library(dplyr)
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
p <- ggplot(colom19, aes(Dia, Contagios, fill = Contagios)) +
  geom_col() +
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  ) + 
  labs(title = "Evolución del Covid-19 en Colombia",
       caption = "Fuente: ISN")

animado<-p + transition_states(Dia, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()
anim_save(animado, filename = 'Contagios6.gif')

####________________________________###
q <- ggplot(colom19, aes(Dia, Contagios, fill = Contagios)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  ) + transition_manual(Dia)+
  labs(subtitle = "Dia: {current_frame}")+
  labs(title = "Evolución del Covid-19 en Colombia",
       caption = "Fuente: ISN")
q
animado2<-q + transition_states(Dia, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()


anim_save('Contagios4.gif')
