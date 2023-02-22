#############################################################################
###Proyecto colciencias: Bases de datos DANE en turismo######################
####################Estadisticas y Graficas de cajas microdatos DANE#########
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
rm(list=ls())
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)  
library(readxl)
library(tidyr)
TURISMO <- read_dta("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Turismo Dane/II.Trimestre/II_Trimestre/TURISMO/TURISMO.dta")
#View(TURISMO)
head(TURISMO)

#Alojamiento
Categorias <- rep("Alojamiento" , 40232)
Alojamiento<-data.frame(TURISMO$P7581S1A1)
final1 <- data.frame(Categorias,Alojamiento)
final1 <- filter(final1, Alojamiento>99)
names(final1)= c("Categorias", "Costos")


#Transporte terrestre (hacia y desde el destino)
Categorias2 <- rep("T. terrestre" , 40232)
tranTerr<-data.frame(TURISMO$P7581S10A1)
final2 <- data.frame(Categorias2,tranTerr)
final2 <- filter(final2, tranTerr>99)
names(final2)= c("Categorias", "Costos")

#Transporte p?blico en el lugar visitado
Categorias3 <- rep("T. visitado" , 40232)
tranVis<-data.frame(TURISMO$P7581S3A1)
final3 <- data.frame(Categorias3,tranVis)
final3 <- filter(final3, tranVis>99)
names(final3)= c("Categorias", "Costos")

#Alimentos y bebidas
Categorias4 <- rep("Alimentaci?n" , 40232)
alim<-data.frame(TURISMO$P7581S4A1)
alim2=alim
final4 <- data.frame(Categorias4,alim, alim2)
#por exageracion de la cifra se recorta creandose un filtro de intervalo
final4 <- filter(final4, alim>99 & alim2<2000000)
final4 <- final4[,-c(3)]
names(final4)= c("Categorias", "Costos")

#Bienes de uso personal
Categorias5 <- rep("B. uso personal" , 40232)
bien<-data.frame(TURISMO$P7581S5A1)
final5 <- data.frame(Categorias5,bien)
final5 <- filter(final5, bien>99)
names(final5)= c("Categorias", "Costos")

#Servicios culturales y recreacionales
Categorias6 <- rep("S. cultural y recreacional" , 40232)
cul<-data.frame(TURISMO$P7581S6A1)
final6 <- data.frame(Categorias6,cul)
final6 <- filter(final6, cul>99)
names(final6)= c("Categorias", "Costos")

# Souvenirs, artesan?as, regalos

Categorias7 <- rep("Obsequios" , 40232)
reg<-data.frame(TURISMO$P7581S7A1)
final7 <- data.frame(Categorias7,reg)
final7 <- filter(final7, reg>99)
names(final7)= c("Categorias", "Costos")

#Otros gastos relacionados con el viaje
Categorias8 <- rep("otros" , 40232)
otro<-data.frame(TURISMO$P7581S8A1)
final8 <- data.frame(Categorias8,otro)
final8 <- filter(final8, otro>99)
names(final8)= c("Categorias", "Costos")


total=rbind(final1,final2, final3, final4, final5, final6, final7 ,final8)
head(total)
ggplot(data = total, aes(x = Categorias, y = Costos)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Categorias), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Aspectos') + 
  ylab('Costos') + guides(fill=FALSE) +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(axis.text.x = element_text(angle = 90))
  #  +  coord_flip()
#quintiles, INTERPRETACION CON EL TRABAJO DE PSICOLOGIA
quantile (final1$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final2$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final3$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final4$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final5$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final6$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final7$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,
quantile (final8$Costos, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))  # 10 partes,

  #link de modificar etiquetas
#https://codeday.me/es/qa/20190326/379777.html
#cambiar ejes + coord_flip()
#Eliminar leyenda
#Ultima parte eliminar decimales
#guides(fill=FALSE)

