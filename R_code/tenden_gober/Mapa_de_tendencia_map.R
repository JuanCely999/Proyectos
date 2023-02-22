##############################################################################
###Mapa de tendencia###########################################
######################Autor: Juan Pablo Cely#################################
######################21/02/2022#################################
###Mapa mundial basico
#https://codeday.me/es/qa/20190805/1193418.html
#opcional#http://rstudio-pubs-static.s3.amazonaws.com/2795_901030c4ef944c7797f39bcdac099d74.html
rm(list=ls())
library(readxl)
library(ggplot2)
library(dplyr)
maps <- read_excel("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/WOS/Imagenes Vantage/Mapa en R/maps.xlsx")

WorldData <- map_data('world')


WorldData %>% filter(region != "Antarctica") -> WorldData
var<-data.frame(table(WorldData$region))
#importante para hacer merge https://pmoracho.github.io/blog/2017/06/26/Combinacion-de-datos-en-R/
final<-merge(x = var, y = maps, by = "Var1", all = TRUE)

WorldData$Publicaciones<-final$suma[match(WorldData$region,var$Var1)]
map<-ggplot(WorldData,aes(x=long, y=lat))+ 
  geom_polygon(aes(group=group, fill=Publicaciones), col=NA, lwd=0)
map+scale_colour_gradient(low="white", high = "grey20")
actual2018<-map+scale_colour_grey()
actual2018

WorldData$Publicaciones<-final$antes[match(WorldData$region,var$Var1)]
ma<-ggplot(WorldData,aes(x=long, y=lat))+ 
  geom_polygon(aes(group=group, fill=Publicaciones), col=NA, lwd=0)
ma+scale_colour_gradient(low="white", high = "grey20")+ theme(legend.position = "none")
antes2011<-ma+scale_colour_grey()
antes2011
#theme(legend.position = "none") #Remover leyenda