##############################################################################
#############################################################################
######################Pobreza y economia####################################
####################NBI rural y urbano. Actividades economicas###############
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)

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
library(reshape2)

##############################################################################
##########################################NBI URBANO Y RURAL##################
##############################################################################
NBI <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
names(NBI)
#CONVERTIR DATOS EN NUMERO
NBIDEP<- filter(NBI, `Código Departamento`==15 )


NBIURB<- filter(NBIDEP, Indicador== "Índice de Necesidades Básicas Insatisfechas - NBI - en el área urbana" )
NBIURB<- data.frame(NBIURB[,c(4,8)])
NBIURB<- rename(NBIURB, c(Urbano=Dato.Numérico))

NBIRUR<- filter(NBIDEP, Indicador== "Índice de Necesidades Básicas Insatisfechas - NBI - en el área rural")
NBIRUR<- data.frame(NBIRUR[,c(8)])
NBIRUR<- rename(NBIRUR, c(Rural=Dato.Numérico))

NBITOTAL<- cbind(NBIURB,NBIRUR)
write.csv(NBITOTAL, file = "NBITOTAL23.csv")

NBITOTAL2 <- read_excel("NBI RURAL Y URBANO/NBIpublica.xlsx")



quantile(NBITOTAL2$Rural, prob = c(0.01, 0.25, 0.5, 0.75, 0.9)) 
quantile(NBITOTAL2$Urbano, prob = c(0.01, 0.25, 0.5, 0.75, 0.9)) 
#Despues del tercer cuartil


ggplot(NBITOTAL2, aes(x= Rural, y = Urbano)) + 
  geom_point(color = dplyr::case_when(NBITOTAL2$Urbano > 9.98 ~ "#1b9e77",
                                      NBITOTAL2$Rural > 21.3 ~ "#1b9e77",
                                      TRUE ~ "#7570b3"), 
             size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = Entidad), data          = subset(NBITOTAL2, Rural > 21.3), 
                  nudge_y       = 32 - subset(NBITOTAL2, Urbano > 9.98)$Urbano,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",) +theme_linedraw()#theme_light()theme_dark()theme_minimal()

#geom_text(aes(color=factor(cyl)))
#https://stackoverflow.com/questions/15624656/label-points-in-geom-point
#https://es.r4ds.hadley.nz/comunicar-con-gr%C3%A1ficos.html   Ecuaciones








##############################################################################
##########################################NBI URBANO Y RURAL##################
##############################################################################
rm(list=ls())
NBI <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
names(NBI)
#CONVERTIR DATOS EN NUMERO
NBIDEP<- filter(NBI, `Código Departamento`==15 )

#NBIDEP11<- filter(NBIDEP, `Año`==2018 )

NBIURB<- filter(NBIDEP, Indicador== "Población en condición de miseria en el área urbana" )
NBIURB<- data.frame(NBIURB[,c(4,8)])
NBIURB<- rename(NBIURB, c(Urbano=Dato.Numérico))

NBIRUR<- filter(NBIDEP, Indicador== "Población en condición de miseria en el área rural")
NBIRUR<- data.frame(NBIRUR[,c(8)])
NBIRUR<- rename(NBIRUR, c(Rural=Dato.Numérico))

NBITOTAL<- cbind(NBIURB,NBIRUR)
write.csv(NBITOTAL, file = "NBITOTAL24.csv")

NBITOTAL2 <- read_excel("NBI RURAL Y URBANO/NBIpublica2.xlsx")



quantile(NBITOTAL2$Rural, prob = c(0.01, 0.25, 0.5, 0.75, 0.9)) 
quantile(NBITOTAL2$Urbano, prob = c(0.01, 0.25, 0.5, 0.75, 0.9)) 
#Despues del tercer cuartil


ggplot(NBITOTAL2, aes(x= Rural, y = Urbano)) + 
  geom_point(color = dplyr::case_when(NBITOTAL2$Urbano > 1 ~ "#1b9e77",
                                      NBITOTAL2$Rural > 3.6 ~ "#1b9e77",
                                      TRUE ~ "#7570b3"), 
             size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = Entidad), data          = subset(NBITOTAL2, Rural > 3.6), 
                  nudge_y       = 32 - subset(NBITOTAL2, Urbano > 1)$Urbano,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",) +theme_linedraw()#theme_light()theme_dark()theme_minimal()

#geom_text(aes(color=factor(cyl)))
#https://stackoverflow.com/questions/15624656/label-points-in-geom-point
#https://es.r4ds.hadley.nz/comunicar-con-gr%C3%A1ficos.html   Ecuaciones




#Mapas

rm(list=ls())
NBI <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
names(NBI)
#CONVERTIR DATOS EN NUMERO
NBIDEP<- filter(NBI, `Código Departamento`==15 )

NBIDEP<- filter(NBIDEP, `Código Entidad`!=15000 )
NBIURB<- filter(NBIDEP, Indicador== "Población en condición de miseria" )


write.csv(NBIURB, file = "NBI24.csv")

#boyaca2 = rename(NBIURB, c(MPIO_CCDGO=`Código Entidad`, Porcentaje=`Dato Numérico`))#la razon es por lo numerico
#boyaca2<- data.frame(boyaca2[,c(1:4,8)])
#write.csv(boyaca2, file = "boyaca_trab.csv")
#boyaca3 <- read.csv("~/Documents/Investigacion/Mapas Pemp/boyaca_trab.csv")
boyaca2$MPIO_CCDGO= as.factor(boyaca2$MPIO_CCDGO)

boyaca_trab <- boyaca_mapa %>% 
  left_join(boyaca2)

directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
boyaca_mapa <- st_read(dsn = "./15_BOYACA/ADMINISTRATIVO/", layer = "MGN_MPIO_POLITICO")

pobreza <- read_excel("~/Documents/Investigacion/Pobreza publicacion/NBI RURAL Y URBANO/pobreza.xlsx")

pobreza$MPIO_CCDGO= as.factor(pobreza$MPIO_CCDGO)
boyaca_trab <- boyaca_mapa %>% 
  left_join(pobreza)

#Adicionar nombres
boyaca_trab <- boyaca_trab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                          st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                             2))


#Mapa de calor por municipio
ggplot(boyaca_trab) +
  geom_sf(aes(fill =Porcentaje))+ xlab("") + ylab("") +
  scale_fill_gradientn(colours = viridis(250, option = "D"))+
  labs(title = "",
       caption = "") +
  annotation_scale() +
  annotation_north_arrow(location='tl')+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(boyaca_trab), datum = NA)+
  theme_void()

#scale_color_manual(values = c("#00AFBB","#E7B800"))+
 # scale_fill_gradient("Número de empresas", low="orange", high="orange4")

coef<- filter(NBI, Indicador== "Coeficiente de Gini" )
Boy<- filter(coef, `Código Entidad`==15000 )
Tun<- filter(coef, `Código Entidad`==15001 )
Boy2<- data.frame(Boy[,c(8)])
Tun2<- data.frame(Tun[,c(8)])

gini<-cbind(Boy2,Tun2)

#Revisar los datos y pasarlos a mano ya que inician desde 2010
Boyacá<-c(0.54,0.54,0.53,0.53,0.53,0.54,0.53,0.51)
Tunja<-c(0.47,0.46,0.47,0.48,0.47,0.47,0.49,0.46)
Año = c("2010", "2011", "2012", "2013", "2014","2015","2016","2017")


cegma1 = data.frame(Año, Boyacá, Tunja)
cegma.long = melt(cegma1)

ggplot(cegma.long, aes(Año, value, fill=variable)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Año, y=value, label=value, vjust=-0.5), position = position_dodge(width=0.9))+ 
  theme_light()+theme(legend.position='bottom')+ xlab("Año") + ylab("Indice de Gini") # para cambiar +coord_flip()
#https://stackoverflow.com/questions/50333537/add-percentage-label-to-geom-bar-chart-in-ggplot2  Seguir
#https://ggplot2-book.org/polishing.html     Para modificar temas





Nacional<-c(-41,-7,-46,-45,-17,17)
Boyacá<-c(-43,-15,-37,-47,-31,+19)
Movilidad<-c("Venta minorista y recreación","Tienda de comestibles y farmacia","Parques","Estaciones de tránsito",
             "Lugares de trabajo","Residencial")


cegma1 = data.frame(Movilidad, Boyacá, Nacional)
cegma.long = melt(cegma1)

ggplot(cegma.long, aes(Movilidad, value, fill=variable)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Movilidad, y=value, label=value, vjust=--1), position = position_dodge(width=0.9))+ 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") +coord_flip()

