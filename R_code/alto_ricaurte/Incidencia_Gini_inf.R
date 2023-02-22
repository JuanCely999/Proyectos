#############################################################################
###Serie de tiempo dinamica ######################
####################Estadisticas y Graficas de cajas MINTIC#########
######################Autor: Juan Pablo Cely#################################
###############################07-04-2020####################################
rm(list=ls())
library(haven)
library(foreign)
library(readxl)
library(dplyr)
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
#############################################################################
#############################################################################
#############################################################################
#############################################################################
# DEPARTAMENTOS######################
#############################################################################
pob <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")

#############################################################################
#CONVERTIR DATOS EN NUMERO

monet<- filter(pob, Indicador== "Incidencia de la pobreza monetaria" )
coef<- filter(pob, Indicador== "Coeficiente de Gini" )


inciden1<- filter(monet, `Código Entidad`==5000 )
inciden2<- filter(monet, `Código Entidad`==8000 )
inciden3<- filter(monet, `Código Entidad`==11001 )
inciden4<- filter(monet, `Código Entidad`==13000 )
inciden5<- filter(monet, `Código Entidad`==15000 )
inciden6<- filter(monet, `Código Entidad`==17000 )
inciden7<- filter(monet, `Código Entidad`==18000 )
inciden8<- filter(monet, `Código Entidad`==19000 )
inciden9<- filter(monet, `Código Entidad`==20000 )
inciden10<- filter(monet, `Código Entidad`==23000 )
inciden11<- filter(monet, `Código Entidad`==25000 )
inciden12<- filter(monet, `Código Entidad`==27000 )
inciden13<- filter(monet, `Código Entidad`==41000 )
inciden14<- filter(monet, `Código Entidad`==44000 )
inciden15<- filter(monet, `Código Entidad`==47000 )
inciden16<- filter(monet, `Código Entidad`==50000 )
inciden17<- filter(monet, `Código Entidad`==52000 )
inciden18<- filter(monet, `Código Entidad`==54000 )
inciden19<- filter(monet, `Código Entidad`==63000 )
inciden20<- filter(monet, `Código Entidad`==66000 )
inciden21<- filter(monet, `Código Entidad`==68000 )
inciden22<- filter(monet, `Código Entidad`==70000 )
inciden23<- filter(monet, `Código Entidad`==73000 )
inciden24<- filter(monet, `Código Entidad`==76000 )
inciden25<- filter(monet, `Código Entidad`==81000 )
inciden26<- filter(monet, `Código Entidad`==85000 )
inciden27<- filter(monet, `Código Entidad`==86000 )
inciden28<- filter(monet, `Código Entidad`==91000 )
inciden29<- filter(monet, `Código Entidad`==94000 )
inciden30<- filter(monet, `Código Entidad`==95000 )
inciden31<- filter(monet, `Código Entidad`==97000 )
inciden32<- filter(monet, `Código Entidad`==99000 )
inciden33<- filter(monet, `Código Entidad`==88000 )




coef1<- filter(coef, `Código Entidad`==5000 )
coef2<- filter(coef, `Código Entidad`==8000 )
coef3<- filter(coef, `Código Entidad`==11001 )
coef4<- filter(coef, `Código Entidad`==13000 )
coef5<- filter(coef, `Código Entidad`==15000 )
coef6<- filter(coef, `Código Entidad`==17000 )
coef7<- filter(coef, `Código Entidad`==18000 )
coef8<- filter(coef, `Código Entidad`==19000 )
coef9<- filter(coef, `Código Entidad`==20000 )
coef10<- filter(coef, `Código Entidad`==23000 )
coef11<- filter(coef, `Código Entidad`==25000 )
coef12<- filter(coef, `Código Entidad`==27000 )
coef13<- filter(coef, `Código Entidad`==41000 )
coef14<- filter(coef, `Código Entidad`==44000 )
coef15<- filter(coef, `Código Entidad`==47000 )
coef16<- filter(coef, `Código Entidad`==50000 )
coef17<- filter(coef, `Código Entidad`==52000 )
coef18<- filter(coef, `Código Entidad`==54000 )
coef19<- filter(coef, `Código Entidad`==63000 )
coef20<- filter(coef, `Código Entidad`==66000 )
coef21<- filter(coef, `Código Entidad`==68000 )
coef22<- filter(coef, `Código Entidad`==70000 )
coef23<- filter(coef, `Código Entidad`==73000 )
coef24<- filter(coef, `Código Entidad`==76000 )
coef25<- filter(coef, `Código Entidad`==81000 )
coef26<- filter(coef, `Código Entidad`==85000 )
coef27<- filter(coef, `Código Entidad`==86000 )
coef28<- filter(coef, `Código Entidad`==91000 )
coef29<- filter(coef, `Código Entidad`==94000 )
coef30<- filter(coef, `Código Entidad`==95000 )
coef31<- filter(coef, `Código Entidad`==97000 )
coef32<- filter(coef, `Código Entidad`==99000 )
coef33<- filter(coef, `Código Entidad`==88000 )



incidencia<-rbind(inciden1, inciden2, inciden3, inciden4, inciden5, inciden6, 
                  inciden7, inciden8, inciden9, inciden10, inciden11, inciden12, 
                  inciden13, inciden14, inciden15, inciden16, inciden17, inciden18,
                  inciden19, inciden20, inciden21, inciden22, inciden23, inciden24,
                  inciden25, inciden26, inciden27, inciden28, inciden29, inciden30, 
                  inciden31, inciden32, inciden33)
write.csv(incidencia, file = "incidencia.csv")

coeficiente<-rbind(coef1, coef2, coef3, coef4, coef5, coef6, 
        coef7, coef8, coef9, coef10, coef11, coef12, 
        coef13, coef14, coef15, coef16, coef17, coef18,
        coef19, coef20, coef21, coef22, coef23, coef24,
        coef25, coef26, coef27, coef28, coef29, coef30, 
        coef31, coef32, coef33)
write.csv(coeficiente, file = "coeficiente.csv")

incidencia2 <- read_excel("~/Documents/Investigacion/Pobreza publicacion/incidencia2.xlsx")

coeficiente2 <- read_excel("~/Documents/Investigacion/Pobreza publicacion/coeficiente2.xlsx")

directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)

#https://vizualdatos.com/como-separar-columna-unificada-coordenadas-variables-longitud-latitud-con-r/
colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")


colom19 <- colombia %>% 
  left_join(incidencia2)

i <- ggplot(colom19)+
  geom_sf(aes(fill=Porcentaje))+  xlab("") + ylab("") + 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(`Año`)+
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Incidencia de la pobreza monetaria en los departamentos de Colombia",
       caption = "@Pedropsalas @JCelyo4") + theme_void()


animate(i, renderer = gifski_renderer("incidencia.gif"))



colom20 <- colombia %>% 
  left_join(coeficiente2)

f <- ggplot(colom20)+
  geom_sf(aes(fill=Porcentaje))+  xlab("") + ylab("") + 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(`Año`)+
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Coeficiente de Gini en los departamentos de Colombia",
       caption = "@Pedropsalas @JCelyo4") + theme_void()


animate(f, renderer = gifski_renderer("coeficiente.gif"))


#############################################################################
#############################################################################
#############################################################################
#############################################################################
# Capitales######################
#############################################################################
rm(list=ls())
pob <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")

#############################################################################
#CONVERTIR DATOS EN NUMERO

monet<- filter(pob, Indicador== "Incidencia de la pobreza monetaria" )
coef<- filter(pob, Indicador== "Coeficiente de Gini" )


inciden1<- filter(monet, `Código Entidad`==5001 )
inciden2<- filter(monet, `Código Entidad`==8001 )
inciden3<- filter(monet, `Código Entidad`==11001 )
inciden4<- filter(monet, `Código Entidad`==13001 )
inciden5<- filter(monet, `Código Entidad`==15001 )
inciden6<- filter(monet, `Código Entidad`==17001 )
inciden7<- filter(monet, `Código Entidad`==18001 )
inciden8<- filter(monet, `Código Entidad`==19001 )
inciden9<- filter(monet, `Código Entidad`==20001 )
inciden10<- filter(monet, `Código Entidad`==23001 )
#inciden11<- filter(monet, `Código Entidad`==25001 ) #cundinamarca
inciden12<- filter(monet, `Código Entidad`==27001 )
inciden13<- filter(monet, `Código Entidad`==41001 )
inciden14<- filter(monet, `Código Entidad`==44001 )
inciden15<- filter(monet, `Código Entidad`==47001 )
inciden16<- filter(monet, `Código Entidad`==50001 )
inciden17<- filter(monet, `Código Entidad`==52001 )
inciden18<- filter(monet, `Código Entidad`==54001 )
inciden19<- filter(monet, `Código Entidad`==63001 )
inciden20<- filter(monet, `Código Entidad`==66001 )
inciden21<- filter(monet, `Código Entidad`==68001 )
inciden22<- filter(monet, `Código Entidad`==70001 )
inciden23<- filter(monet, `Código Entidad`==73001 )
inciden24<- filter(monet, `Código Entidad`==76001 )
inciden25<- filter(monet, `Código Entidad`==81001 )
inciden26<- filter(monet, `Código Entidad`==85001 )
inciden27<- filter(monet, `Código Entidad`==86001 )
inciden28<- filter(monet, `Código Entidad`==91001 )
inciden29<- filter(monet, `Código Entidad`==94001 )
inciden30<- filter(monet, `Código Entidad`==95001 )
inciden31<- filter(monet, `Código Entidad`==97001 )
inciden32<- filter(monet, `Código Entidad`==99001 )
#inciden33<- filter(monet, `Código Entidad`==88001 )




coef1<- filter(coef, `Código Entidad`==5001 )
coef2<- filter(coef, `Código Entidad`==8001 )
coef3<- filter(coef, `Código Entidad`==11001 )
coef4<- filter(coef, `Código Entidad`==13001 )
coef5<- filter(coef, `Código Entidad`==15001 )
coef6<- filter(coef, `Código Entidad`==17001 )
coef7<- filter(coef, `Código Entidad`==18001 )
coef8<- filter(coef, `Código Entidad`==19001 )
coef9<- filter(coef, `Código Entidad`==20001 )
coef10<- filter(coef, `Código Entidad`==23001 )
#coef11<- filter(coef, `Código Entidad`==25001 ) #cundinamarca
coef12<- filter(coef, `Código Entidad`==27001 )
coef13<- filter(coef, `Código Entidad`==41001 )
coef14<- filter(coef, `Código Entidad`==44001 )
coef15<- filter(coef, `Código Entidad`==47001 )
coef16<- filter(coef, `Código Entidad`==50001 )
coef17<- filter(coef, `Código Entidad`==52001 )
coef18<- filter(coef, `Código Entidad`==54001 )
coef19<- filter(coef, `Código Entidad`==63001 )
coef20<- filter(coef, `Código Entidad`==66001 )
coef21<- filter(coef, `Código Entidad`==68001 )
coef22<- filter(coef, `Código Entidad`==70001 )
coef23<- filter(coef, `Código Entidad`==73001 )
coef24<- filter(coef, `Código Entidad`==76001 )
coef25<- filter(coef, `Código Entidad`==81001 )
coef26<- filter(coef, `Código Entidad`==85001 )
coef27<- filter(coef, `Código Entidad`==86001 )
coef28<- filter(coef, `Código Entidad`==91001 )
coef29<- filter(coef, `Código Entidad`==94001 )
coef30<- filter(coef, `Código Entidad`==95001 )
coef31<- filter(coef, `Código Entidad`==97001 )
coef32<- filter(coef, `Código Entidad`==99001 )
#coef33<- filter(coef, `Código Entidad`==88001 )



incidencia<-rbind(inciden1, inciden2, inciden3, inciden4, inciden5, inciden6, 
                  inciden7, inciden8, inciden9, inciden10, inciden12, 
                  inciden13, inciden14, inciden15, inciden16, inciden17, inciden18,
                  inciden19, inciden20, inciden21, inciden22, inciden23, inciden24,
                  inciden25, inciden26, inciden27, inciden28, inciden29, inciden30, 
                  inciden31, inciden32)
write.csv(incidencia, file = "ciuincidencia.csv")

coeficiente<-rbind(coef1, coef2, coef3, coef4, coef5, coef6, 
                   coef7, coef8, coef9, coef10, coef12, 
                   coef13, coef14, coef15, coef16, coef17, coef18,
                   coef19, coef20, coef21, coef22, coef23, coef24,
                   coef25, coef26, coef27, coef28, coef29, coef30, 
                   coef31, coef32)
write.csv(coeficiente, file = "ciucoeficiente.csv")

incidencia2 <- read_excel("~/Documents/Investigacion/Pobreza publicacion/ciuincidencia2.xlsx")

coeficiente2 <- read_excel("~/Documents/Investigacion/Pobreza publicacion/ciucoeficiente2.xlsx")

directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)

#https://vizualdatos.com/como-separar-columna-unificada-coordenadas-variables-longitud-latitud-con-r/
colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")


colom19 <- colombia %>% 
  left_join(incidencia2)

i <- ggplot(colom19)+
  geom_sf(aes(fill=Porcentaje))+  xlab("") + ylab("") + 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(`Año`)+
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Incidencia de la pobreza monetaria en las ciudades de Colombia",
       caption = "@Pedropsalas @JCelyo4") + theme_void()


animate(i, renderer = gifski_renderer("ciuincidencia.gif"))



colom20 <- colombia %>% 
  left_join(coeficiente2)

f <- ggplot(colom20)+
  geom_sf(aes(fill=Porcentaje))+  xlab("") + ylab("") + 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(`Año`)+
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Coeficiente de Gini en las ciudades de Colombia",
       caption = "@Pedropsalas @JCelyo4") + theme_void()


animate(f, renderer = gifski_renderer("ciucoeficiente.gif"))



