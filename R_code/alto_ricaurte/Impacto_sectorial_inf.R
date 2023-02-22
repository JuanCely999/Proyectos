###############Ejercicio: Mapas interactivos de Colombia por COVID-19#################
#############################################################################
######################Autor: Juan Pablo Cely#################################
###############################14-05-2020####################################
rm(list=ls())
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
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
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)

#https://arcruz0.github.io/libroadp/mapas.html
directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
#https://vizualdatos.com/como-separar-columna-unificada-coordenadas-variables-longitud-latitud-con-r/
colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")

perdidas <- read_excel("~/Documents/Investigacion/Impacto sectorial en Boyaca/perdidas.xlsx")

colombia33 <- colombia %>% 
  left_join(perdidas)

colombia33 <- cbind(colombia33, st_coordinates(st_centroid(colombia33$geometry)))

#OPCIÓN 1
ggplot(colombia33)+
  geom_sf(aes(fill=perdidas))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = ingresos),
             colour = 'red', alpha = .5) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(30, 35, 40, 45, 50, 55))+ 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
 labs(size = 'ingresos')

#OPCIÓN 2 MUCHO MEJOR

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=perdidas))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = ingresos),
             colour = 'red', alpha = .5) +
  scale_size_continuous("Participación de ingreso \n por trabajador", range = c(1, 12), 
                        breaks = c(30, 35, 40, 45, 50, 55))+
  scale_fill_gradient("Pérdidas mensuales \n (miles de millones)", low="orange", high="orange4") +  # nombres en colors()
  theme_classic()

###################################Sectores###########
names(colombia33)
a<-ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=Agricultura))+  xlab("") + ylab("")+ 
  scale_fill_gradient("", low="orange", high="orange4") +  # nombres en colors()
  theme_classic()+labs(subtitle = "Agricultura")

b<-ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=Mineria))+  xlab("") + ylab("")+ 
  scale_fill_gradient("", low="orange", high="orange4") +  # nombres en colors()
  theme_classic()+labs(subtitle = "Mineria")

c<-ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=Manufactura))+  xlab("") + ylab("")+ 
  scale_fill_gradient("", low="orange", high="orange4") +  # nombres en colors()
  theme_classic()+labs(subtitle = "Manufactura")

d<-ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=Servicios))+  xlab("") + ylab("")+ 
  scale_fill_gradient("", low="orange", high="orange4") +  # nombres en colors()
  theme_classic()+labs(subtitle = "Servicios")

grid.arrange(a, b, c, d)



