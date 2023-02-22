##############################################################################
###Proyecto colciencias: Turismo y gobernanza################################
#########################Graficas en violin de los comentarios en Twitter#####
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
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/informe/h"
setwd(dir.resul)
#####################################Turismo y Gobernanza##################
###########################################################################
afrGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/afrGen.csv")
asiaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/asiaGen.csv")
colGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/colGen.csv")
europaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/europaGen.csv")
lameGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/lameGen.csv")
norteamericaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/norteamericaGen.csv")
oceaniaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/oceaniaGen.csv")

#sapply(afrGen, function(x) sum(is.na(x))) Posible opcion para borrar las filas NA
#afrGen<- afrGen[!is.na(afrGen$word_count),]

dim(colGen)
Lugar <- rep("Colombia" , 395)
final1 <- colGen$sentiment
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final1 <- data.frame(final1,Lugar)
final1 <- filter(final1, final1!=0)
names(final1)= c("Sentimiento", "Lugar")
png("col1.png",width = 448,height = 289)
qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(lameGen)
Lugar <- rep("Suram?rica" , 418)
final2 <- lameGen$sentiment
final2 <- data.frame(final2,Lugar)
final2 <- filter(final2, final2!=0)
names(final2)= c("Sentimiento", "Lugar")
png("lat1.png",width = 448,height = 289)
qplot(final2$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(norteamericaGen)
Lugar <- rep("Norteam?rica" , 21097)
final3 <- norteamericaGen$sentiment
final3 <- data.frame(final3,Lugar)
final3 <- filter(final3, final3!=0)
names(final3)= c("Sentimiento", "Lugar")
png("nor1.png",width = 448,height = 289)
qplot(final3$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(asiaGen)
Lugar <- rep("Sur de Asia" , 16104)
final4 <- asiaGen$sentiment
final4 <- data.frame(final4,Lugar)
final4 <- filter(final4, final4!=0)
names(final4)= c("Sentimiento", "Lugar")
png("asia1.png",width = 448,height = 289)
qplot(final4$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(afrGen)
Lugar <- rep("Sur de Africa" , 7239)
final5 <- afrGen$sentiment
final5 <- data.frame(final5,Lugar)
final5 <- filter(final5, final5!=0)
names(final5)= c("Sentimiento", "Lugar")
png("afr1.png",width = 448,height = 289)
qplot(final5$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(europaGen)
Lugar <- rep("Europa" , 17424)
final6 <- europaGen$sentiment
final6 <- data.frame(final6,Lugar)
final6 <- filter(final6, final6!=0)
names(final6)= c("Sentimiento", "Lugar")
png("eur1.png",width = 448,height = 289)
qplot(final6$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(oceaniaGen)
Lugar <- rep("Oceania" , 996)
final7 <- oceaniaGen$sentiment
final7 <- data.frame(final7,Lugar)
final7 <- filter(final7, final7!=0)
names(final7)= c("Sentimiento", "Lugar")
png("oce1.png",width = 448,height = 289)
qplot(final7$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

total=rbind(final1,final2, final3, final4, final5, final6, final7)

ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
####Estadisticas#######
summarise(final1, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final1$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final2, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                              
summarise(final3, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final4, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final5, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final6, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final7, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )





rm(list=ls())
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)  
library(readxl)
library(tidyr)
##############################################################
#####################################Turismo ##################
###########################################################################
afrGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/afrTur.csv")
asiaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/asiaTur.csv")
colGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/colTur.csv")
europaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/europaTur.csv")
lameGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/lameTur.csv")
norteamericaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/norteamericaTur.csv")
oceaniaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/oceaniaTur.csv")

#sapply(afrGen, function(x) sum(is.na(x))) Posible opcion para borrar las filas NA
#afrGen<- afrGen[!is.na(afrGen$word_count),]

dim(colGen)
Lugar <- rep("Colombia" , 258)
final1 <- colGen$sentiment
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final1 <- data.frame(final1,Lugar)
final1 <- filter(final1, final1!=0)
names(final1)= c("Sentimiento", "Lugar")
png("col2.png",width = 448,height = 289)
qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(lameGen)
Lugar <- rep("Suram?rica" , 235)
final2 <- lameGen$sentiment
final2 <- data.frame(final2,Lugar)
final2 <- filter(final2, final2!=0)
names(final2)= c("Sentimiento", "Lugar")
png("lat2.png",width = 448,height = 289)
qplot(final2$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(norteamericaGen)
Lugar <- rep("Norteam?rica" , 9893)
final3 <- norteamericaGen$sentiment
final3 <- data.frame(final3,Lugar)
final3 <- filter(final3, final3!=0)
names(final3)= c("Sentimiento", "Lugar")
png("nor2.png",width = 448,height = 289)
qplot(final3$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(asiaGen)
Lugar <- rep("Sur de Asia" , 6115)
final4 <- asiaGen$sentiment
final4 <- data.frame(final4,Lugar)
final4 <- filter(final4, final4!=0)
names(final4)= c("Sentimiento", "Lugar")
png("asia2.png",width = 448,height = 289)
qplot(final4$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(afrGen)
Lugar <- rep("Sur de Africa" , 3217)
final5 <- afrGen$sentiment
final5 <- data.frame(final5,Lugar)
final5 <- filter(final5, final5!=0)
names(final5)= c("Sentimiento", "Lugar")
png("afr2.png",width = 448,height = 289)
qplot(final5$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(europaGen)
Lugar <- rep("Europa" , 9478)
final6 <- europaGen$sentiment
final6 <- data.frame(final6,Lugar)
final6 <- filter(final6, final6!=0)
names(final6)= c("Sentimiento", "Lugar")
png("eur2.png",width = 448,height = 289)
qplot(final6$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(oceaniaGen)
Lugar <- rep("Oceania" , 624)
final7 <- oceaniaGen$sentiment
final7 <- data.frame(final7,Lugar)
final7 <- filter(final7, final7!=0)
names(final7)= c("Sentimiento", "Lugar")
png("oce2.png",width = 448,height = 289)
qplot(final7$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

total=rbind(final1,final2, final3, final4, final5, final6, final7)

ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
####Estadisticas#######
summarise(final1, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final1$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final2, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                              
summarise(final3, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final4, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final5, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final6, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final7, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))











rm(list=ls())
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)  
library(readxl)
library(tidyr)
##############################################################
#####################################Gobernanza ##################
###########################################################################
afrGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/afrGov.csv")
asiaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/asiaGov.csv")
colGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/colGov.csv")
europaGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/europaGov.csv")
lameGen<- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/lameGov.csv")
norteamericaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/norteamericaGov.csv")
oceaniaGen <- read.csv("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Ws paises/oceaniaGov.csv")

#sapply(afrGen, function(x) sum(is.na(x))) Posible opcion para borrar las filas NA
#afrGen<- afrGen[!is.na(afrGen$word_count),]

dim(colGen)
Lugar <- rep("Colombia" , 137)
final1 <- colGen$sentiment
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final1 <- data.frame(final1,Lugar)
final1 <- filter(final1, final1!=0)
names(final1)= c("Sentimiento", "Lugar")
png("col3.png",width = 448,height = 289)
qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(lameGen)
Lugar <- rep("Suram?rica" , 183)
final2 <- lameGen$sentiment
final2 <- data.frame(final2,Lugar)
final2 <- filter(final2, final2!=0)
names(final2)= c("Sentimiento", "Lugar")
png("lat3.png",width = 448,height = 289)
qplot(final2$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(norteamericaGen)
Lugar <- rep("Norteam?rica" , 11204)
final3 <- norteamericaGen$sentiment
final3 <- data.frame(final3,Lugar)
final3 <- filter(final3, final3!=0)
names(final3)= c("Sentimiento", "Lugar")
png("nor3.png",width = 448,height = 289)
qplot(final3$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(asiaGen)
Lugar <- rep("Sur de Asia" , 9989)
final4 <- asiaGen$sentiment
final4 <- data.frame(final4,Lugar)
final4 <- filter(final4, final4!=0)
names(final4)= c("Sentimiento", "Lugar")
png("asia3.png",width = 448,height = 289)
qplot(final4$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()


dim(afrGen)
Lugar <- rep("Sur de Africa" , 4022)
final5 <- afrGen$sentiment
final5 <- data.frame(final5,Lugar)
final5 <- filter(final5, final5!=0)
names(final5)= c("Sentimiento", "Lugar")
png("afr3.png",width = 448,height = 289)
qplot(final5$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(europaGen)
Lugar <- rep("Europa" , 7946)
final6 <- europaGen$sentiment
final6 <- data.frame(final6,Lugar)
final6 <- filter(final6, final6!=0)
names(final6)= c("Sentimiento", "Lugar")
png("eur3.png",width = 448,height = 289)
qplot(final6$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

dim(oceaniaGen)
Lugar <- rep("Oceania" , 372)
final7 <- oceaniaGen$sentiment
final7 <- data.frame(final7,Lugar)
final7 <- filter(final7, final7!=0)
names(final7)= c("Sentimiento", "Lugar")
png("oce3.png",width = 448,height = 289)
qplot(final7$Sentimiento,   geom="histogram",binwidth=0.1) + 
  xlab('Puntaje del sentimiento') + 
  ylab('Frecuencia')
dev.off()

total=rbind(final1,final2, final3, final4, final5, final6, final7)

ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
####Estadisticas#######
summarise(final1, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final1$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final2, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                              
summarise(final3, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final4, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
summarise(final5, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final6, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                )
summarise(final7, media = mean(Sentimiento), sd= sd(Sentimiento))
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))




