##############################################################################
###Estudio de mercado Proyecto Visota#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
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
library(scales)
library(plotly)
#Estructura_CEED <- read_sav("~/Documents/Investigacion/Analisis de mercado Villota/Datos/Estructura CEED/Estructura CEED.sav")
#EAC_2018 <- read_sav("~/Documents/Investigacion/Analisis de mercado Villota/Datos/EAC_2018/EAC_2018.sav")


Empresas <- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Empresas.xlsx")
Empresas2 <- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Empresas2.xlsx")

totempresas<-rbind(Empresas,Empresas2)
totempresas[is.na(totempresas)] <- 0
names(totempresas)
#empresasnit<-totempresas[!duplicated(totempresas$NIT),] #Se repiten por diferentes municipios
#colombia<- filter(empresasnit, CIIU_ID_CIIU_4==2392 )
#Boyaca<- filter(colombia, MUNI_ID_DPTO==15 )

totempresas = rename(totempresas, c(MPIO_CCDGO2=MUNI_ID_MPIO, DPTO=MUNI_ID_DPTO))

MPIO_CCDGO=paste(totempresas$DPTO,totempresas$MPIO_CCDGO2,sep="")
MPIO_CCDGO=data.frame(MPIO_CCDGO)

totempresas=cbind(totempresas,MPIO_CCDGO)

#colombia2<- filter(totempresas, CIIU_ID_CIIU_4==2392 )
#cab <- colombia2 %>% group_by(DPTO) %>% summarise(DPTO2 = n(CIIU_ID_CIIU_4))
#Boyaca2<- filter(colombia2, MUNI_ID_DPTO==15 )
#2392 Estudio de ladrillos
(colo3<-totempresas %>%
  filter(CIIU_ID_CIIU_4 == 2824) %>%
  count(DPTO, sort = TRUE))

colo3<-data.frame(colo3)
names(colo3)

#colo3<-rename(colo3,Porcentaje=n)
colo3<- colo3 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) 


#####Insertar Mapa
directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)

colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")

colombiafab <- colombia %>% 
  left_join(colo3)

colombiafab[is.na(colombiafab)] <- 0

colombiafabric <- colombiafab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                          st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                             2))
colombiafab <- cbind(colombiafab, st_coordinates(st_centroid(colombiafab$geometry)))#importante

ggplot(colombiafab)+ # tamaño 1000*800
  geom_sf(aes(fill=n))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = Porcentaje),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Número de empresas", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(1,3,5,7,9,12,16))+ # nombres en colors()
  geom_text_repel(colombiafabric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(colombiafabric), datum = NA)+
  theme_void()


#2392 Ladrillos
##########Boyaca#####################
#####################################
boyaca<-filter(totempresas, DPTO==15 )
#nomboy<-filter(boyaca, CIIU_ID_CIIU_4==2392 )
(boya3<-boyaca %>%
    filter(CIIU_ID_CIIU_4 == 2824) %>%
    count(MPIO_CCDGO, sort = TRUE))

boya3<-data.frame(boya3)
names(boya3)

#boya3<-rename(boya3,Porcentaje=n)
boya3<- boya3 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) 
boyaca_mapa <- st_read(dsn = "./15_BOYACA/ADMINISTRATIVO/", layer = "MGN_MPIO_POLITICO")

boyacafab <- boyaca_mapa %>% 
  left_join(boya3)

boyacafab[is.na(boyacafab)] <- 0

boyacafabric <- boyacafab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                             st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                                2))
boyacafab <- cbind(boyacafab, st_coordinates(st_centroid(boyacafab$geometry)))#importante

ggplot(boyacafab)+ # tamaño 1000*800
  geom_sf(aes(fill=n))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = Porcentaje),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Número de empresas", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(10,20,30,40,50,60))+ # nombres en colors()
  geom_text_repel(boyacafabric, mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(boyacafabric), datum = NA)+
  theme_void()
