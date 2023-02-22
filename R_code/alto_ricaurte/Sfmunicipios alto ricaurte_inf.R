###############Ejercicio: Mapas Municipios del Alto Ricaurte Original#################
#############################################################################
######################Autor: Juan Pablo Cely#################################
###############################04-04-2020####################################
#procurar eliminar el documento Diapositivas NBI
#https://arcruz0.github.io/libroadp/mapas.html
#TAREAS HACER POR ESTRADOS CADA MANZANA Y LAS VEREDAS (POR MEDIO DEL MAPA POLITICO)
Codigos
15293 GACHANTIVÁ Gachantivá
15808 TINJACÁ Tinjacá
15407 VILLA DE LEYVA Villa De Leyva
15776 SUTAMARCHÁN Sutamarchán
15696 SANTA SOFÍA Santa Sofía
15600 RÁQUIRA Ráquira
15638 SÁCHICA Sáchica


#install.packages("paqueteadp")
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

directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
boyaca <-  st_read(dsn = "./15_BOYACA/Administrativo/", layer = "MGN_MPIO_POLITICO")
ggplot(data = boyaca)+
  geom_sf()

gachantiva <- boyaca[boyaca$MPIO_CCDGO==15293,]
tinjaca <- boyaca[boyaca$MPIO_CCDGO==15808,]
villa <- boyaca[boyaca$MPIO_CCDGO==15407,]
suta <- boyaca[boyaca$MPIO_CCDGO==15776,]
santa <- boyaca[boyaca$MPIO_CCDGO==15696,]
raquira <- boyaca[boyaca$MPIO_CCDGO==15600,]
sach <- boyaca[boyaca$MPIO_CCDGO==15638,]
boyaca2<- rbind(gachantiva,tinjaca,villa,suta,santa,raquira,sach)
view(boyaca2)
ggplot(data = boyaca2)+
  geom_sf()

#Adicionar nombres
boyaca2 <- boyaca2 %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
         st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                              2))
head(boyaca2)

ggplot(data = boyaca2) +
  geom_sf()+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 4, min.segment.length = 0)

#Estratificacion por veredas del alto ricaurte
#Formatos para estratificar
mz_boyaca <-  st_read(dsn = "./15_BOYACA/URBANO/", layer = "MGN_URB_MANZANA")
vivienda <- read_dta("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/15_Boyaca/15_Boyaca/15_Boyaca_DTA/CNPV2018_1VIV_A2_15.DTA")
georeferencia <- read_dta("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/15_Boyaca/15_Boyaca/15_Boyaca_DTA/CNPV2018_MGN_A2_15.DTA")

#otra forma de hacer merge
ubi_vivi<-merge(x = vivienda, y = georeferencia, by = "cod_encuestas", all = TRUE)

gach<- filter(ubi_vivi, u_mpio.x==293)
table(gach$u_mza, gach$va1_estrato)#Descargar

tinja<- filter(ubi_vivi, u_mpio.x==808)
table(tinja$u_mza, tinja$va1_estrato)#Descargar

villa<- filter(ubi_vivi, u_mpio.x==407)
table(villa$u_mza, villa$va1_estrato)#Descargar

suta<- filter(ubi_vivi, u_mpio.x==776)
table(suta$u_mza, suta$va1_estrato)#Descargar

santa<- filter(ubi_vivi, u_mpio.x==696)
table(santa$u_mza, santa$va1_estrato)#Descargar

raqui<- filter(ubi_vivi, u_mpio.x==600)
table(raqui$u_mza, raqui$va1_estrato)#Descargar

sach<- filter(ubi_vivi, u_mpio.x==638)
table(sach$u_mza, sach$va1_estrato)#Descargar

#Manera de hacer un merge en las veredas con estratos
gach <- mz_gachantiva %>% 
  left_join(Estratos)
veredas <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Georeferencias/veredas.xlsx")
veredas2 <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Georeferencias/vereda2.xlsx")
#modificar a factores
veredas$MPIO_CCDGO <-as.character(as.numeric(veredas$MPIO_CCDGO))

veredas2$MPIO_CCDGO <-as.character(as.numeric(veredas2$MPIO_CCDGO))

#Manera de hacer un merge
vere1 <- boyaca2 %>% 
  left_join(veredas)

#Version original para tratar series de tiempo
vere2 <- boyaca2 %>% 
  left_join(veredas2)


  
a<-ggplot(vere2 %>%  filter(Etiquetas == "Estrato1")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 1")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()


b <- ggplot(vere2 %>%  filter(Etiquetas == "Estrato2")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 2")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()



c <- ggplot(vere2 %>%  filter(Etiquetas == "Estrato3")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 3")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()



d <- ggplot(vere2 %>%  filter(Etiquetas == "Estrato4")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 4")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()



e <- ggplot(vere2 %>%  filter(Etiquetas == "Estrato5")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 5")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()



f <- ggplot(vere2 %>%  filter(Etiquetas == "Estrato6")) +
  geom_sf(aes(fill = Estratos))+
  labs(subtitle = "Estrato 6")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)+
  theme_void()




grid.arrange(a,b,c,d,e,f)

#continuar con vere 2
install.packages("gifski")
library(gifski)
library(gganimate)

i <- ggplot(vere2)+
  geom_sf(aes(fill=Estratos))+
  transition_manual(Etiquetas)+
  labs(subtitle = "Estrato: {current_frame}")+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)


animate(i)
animate(i, renderer = gifski_renderer("estratos2.gif"))



boy<- data.frame(boyaca2[,c(1:2)])
names(final)= c("Sentimiento", "Lugar")


















#Estratificacion por manzanas en Gachantiva
mz_gachantiva <- mz_boyaca[mz_boyaca$MPIO_CCDGO==15293,]

ggplot(data = mz_gachantiva)+
  geom_sf()
#Merge estratos
vivi_gac<- filter(vivienda, u_mpio==293 )
view(vivi_gac)
georef<- filter(georeferencia, u_mpio==293 )
view(georef)
estratos<-merge(x = vivi_gac, y = georef, by = "cod_encuestas", all = TRUE)
view(estratos)
names(estratos)
table(estratos$u_mza, estratos$va1_estrato)

Estratos <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Georeferencias/Gachantiva.xlsx")

#Manera de hacer un merge
gach <- mz_gachantiva %>% 
  left_join(Estratos)
#Mapa de calor por manzana
ggplot(gach %>% filter(Estrato1 != "NA")) +
  geom_sf(aes(fill = Estrato1))

#union de todos los estratos
a<-ggplot(gach %>% filter(Estrato1 != "NA")) +
  geom_sf(aes(fill = Estrato1))
b<-ggplot(gach %>% filter(Estrato2 != "NA")) +
  geom_sf(aes(fill = Estrato2))
c<-ggplot(gach %>% filter(Estrato3 != "NA")) +
  geom_sf(aes(fill = Estrato3))

grid.arrange(a,b,c)



#Veredas 
mz_boyaca2 <- st_read(dsn = "./15_BOYACA/MGN/", layer = "MGN_RUR_SECCION")
rur_gachantiva <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==293,]
rur_tinj <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==808,]
rur_villa <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==407,]
rur_suta <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==776,]
rur_santa <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==696,]
rur_raquira <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==600,]
rur_sach <- mz_boyaca2[mz_boyaca2$MPIO_CCDGO==638,]


#Grafica de veredas
ggplot(rur_gachantiva,) +
  geom_sf(data=rur_gachantiva,fill = "greenyellow", color = "black") +
  geom_sf(data=rur_tinj,fill = "pink", color = "black") +
  geom_sf(data=rur_villa,fill = "greenyellow", color = "black") +
  geom_sf(data=rur_suta,fill = "darkgoldenrod", color = "black") +
  geom_sf(data=rur_santa,fill = "chartreuse", color = "black") +
  geom_sf(data=rur_raquira,fill = "cornsilk", color = "black") +
  geom_sf(data=rur_sach,fill = "brown1", color = "black") +  theme_void()
#http://www.diegocalvo.es/ejemplo-de-grafico-de-burbujas-en-r/ Adicionar burbujas



##########################################################################
##########################################################################
##########ORGANIZACIONES COMUNALES ALTO RICAURTE##########################

directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
boyaca <-  st_read(dsn = "./15_BOYACA/Administrativo/", layer = "MGN_MPIO_POLITICO")


gachantiva <- boyaca[boyaca$MPIO_CCDGO==15293,]
raquira <- boyaca[boyaca$MPIO_CCDGO==15600,]
sach <- boyaca[boyaca$MPIO_CCDGO==15638,]
santa <- boyaca[boyaca$MPIO_CCDGO==15696,]
suta <- boyaca[boyaca$MPIO_CCDGO==15776,]
tinjaca <- boyaca[boyaca$MPIO_CCDGO==15808,]
villa <- boyaca[boyaca$MPIO_CCDGO==15407,]
boyaca2<- rbind(gachantiva,tinjaca,villa,suta,santa,raquira,sach)
ggplot(data = boyaca2)+geom_sf()

comunal <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Organizacion comunal/comunal.xlsx")

comunal <- transform(comunal, MPIO_CCDGO = as.factor(MPIO_CCDGO)) 
               # char_fac = as.numeric(char_fac))

ricaurte <- boyaca2 %>% 
  left_join(comunal)

ricaurte <- ricaurte %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                  st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                     2))

a<-ggplot(ricaurte) +
  geom_sf(aes(fill =Presidentes))+ xlab("") + ylab("") + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)

b<-ggplot(ricaurte) +
  geom_sf(aes(fill =ESAL))+ xlab("") + ylab("") +
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0)

  

grid.arrange(a,b)



