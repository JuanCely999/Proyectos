##############################################################################
###Capitulo 2 Elaboración datos panel################################
######################Autor: Juan Pablo Cely#################################
###############################25-07-2020####################################
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)


cat("\f")
rm(list = ls())
ciclos <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/MesesCorridoCol2.dta")


tiempo<- 1:120
tiempo<- data_frame(tiempo)

#################Antioquia
anti<- data.frame(ciclos[,c(1,27,52)])
anti2 <- rep("Antioquia" , 120)
anti3<-cbind(tiempo,anti2,anti)
names(anti3)= c("tiempo","depar", "mes","infla","cmar")

#################Atlántico  
atla<- data.frame(ciclos[,c(1,28,54)])
atla2 <- rep("Atlántico", 120)
atla3<-cbind(tiempo,atla2,atla)
names(atla3)= c("tiempo","depar", "mes","infla","cmar")

#################Bogotá  
bogo<- data.frame(ciclos[,c(1,29,56)])
bogo2 <- rep("Bogotá", 120)
bogo3<-cbind(tiempo,bogo2,bogo)
names(bogo3)= c("tiempo","depar", "mes","infla","cmar")

#################Bolívar  
boli<- data.frame(ciclos[,c(1,30,58)])
boli2 <- rep("Bolívar", 120)
boli3<-cbind(tiempo,boli2,boli)
names(boli3)= c("tiempo","depar", "mes","infla","cmar")

#################Boyacá  
boya<- data.frame(ciclos[,c(1,31,60)])
boya2 <- rep("Boyacá", 120)
boya3<-cbind(tiempo,boya2,boya)
names(boya3)= c("tiempo","depar", "mes","infla","cmar")

#################Caldas  
cald<- data.frame(ciclos[,c(1,32,62)])
cald2 <- rep("Caldas", 120)
cald3<-cbind(tiempo,cald2,cald)
names(cald3)= c("tiempo","depar", "mes","infla","cmar")

#################Caquetá  
caqu<- data.frame(ciclos[,c(1,33,64)])
caqu2 <- rep("Caquetá", 120)
caqu3<-cbind(tiempo,caqu2,caqu)
names(caqu3)= c("tiempo","depar", "mes","infla","cmar")

#################Cauca  
cauc<- data.frame(ciclos[,c(1,34,66)])
cauc2 <- rep("Cauca", 120)
cauc3<-cbind(tiempo,cauc2,cauc)
names(cauc3)= c("tiempo","depar", "mes","infla","cmar")


#################Cesar  
cesa<- data.frame(ciclos[,c(1,35,68)])
cesa2 <- rep("Cesar", 120)
cesa3<-cbind(tiempo,cesa2,cesa)
names(cesa3)= c("tiempo","depar", "mes","infla","cmar")

#################Córdoba  
cord<- data.frame(ciclos[,c(1,36,70)])
cord2 <- rep("Córdoba", 120)
cord3<-cbind(tiempo,cord2,cord)
names(cord3)= c("tiempo","depar", "mes","infla","cmar")

#################Chocó  
choc<- data.frame(ciclos[,c(1,37,74)])
choc2 <- rep("Chocó", 120)
choc3<-cbind(tiempo,choc2,choc)
names(choc3)= c("tiempo","depar", "mes","infla","cmar")

#################Cundinamarca  
cund<- data.frame(ciclos[,c(1,29,72)])
cund2 <- rep("Cundinamarca", 120)
cund3<-cbind(tiempo,cund2,cund)
names(cund3)= c("tiempo","depar", "mes","infla","cmar")

#################Huila  
huil<- data.frame(ciclos[,c(1,38,76)])
huil2 <- rep("Huila", 120)
huil3<-cbind(tiempo,huil2,huil)
names(huil3)= c("tiempo","depar", "mes","infla","cmar")

#################La Guajira  
lagua<- data.frame(ciclos[,c(1,39,78)])
lagua2 <- rep("La Guajira", 120)
lagua3<-cbind(tiempo,lagua2,lagua)
names(lagua3)= c("tiempo","depar", "mes","infla","cmar")

#################Magdalena  
magd<- data.frame(ciclos[,c(1,40,80)])
magd2 <- rep("Magdalena", 120)
magd3<-cbind(tiempo,magd2,magd)
names(magd3)= c("tiempo","depar", "mes","infla","cmar")

#################Meta  
meta<- data.frame(ciclos[,c(1,41,82)])
meta2 <- rep("Meta", 120)
meta3<-cbind(tiempo,meta2,meta)
names(meta3)= c("tiempo","depar", "mes","infla","cmar")

#################Nariño  
nari<- data.frame(ciclos[,c(1,42,84)])
nari2 <- rep("Nariño", 120)
nari3<-cbind(tiempo,nari2,nari)
names(nari3)= c("tiempo","depar", "mes","infla","cmar")

#################N. Santander  
nort<- data.frame(ciclos[,c(1,43,86)])
nort2 <- rep("N. Santander ", 120)
nort3<-cbind(tiempo,nort2,nort)
names(nort3)= c("tiempo","depar", "mes","infla","cmar")

#################Quindío  
quin<- data.frame(ciclos[,c(1,44,88)])
quin2 <- rep("Quindío", 120)
quin3<-cbind(tiempo,quin2,quin)
names(quin3)= c("tiempo","depar", "mes","infla","cmar")

#################Risaralda  
risa<- data.frame(ciclos[,c(1,45,90)])
risa2 <- rep("Risaralda", 120)
risa3<-cbind(tiempo,risa2,risa)
names(risa3)= c("tiempo","depar", "mes","infla","cmar")

################Santander  
sant<- data.frame(ciclos[,c(1,46,92)])
sant2 <- rep("Santander", 120)
sant3<-cbind(tiempo,sant2,sant)
names(sant3)= c("tiempo","depar", "mes","infla","cmar")

################Sucre  
sucr<- data.frame(ciclos[,c(1,47,94)])
sucr2 <- rep("Sucre", 120)
sucr3<-cbind(tiempo,sucr2,sucr)
names(sucr3)= c("tiempo","depar", "mes","infla","cmar")

################Tolima  
toli<- data.frame(ciclos[,c(1,48,96)])
toli2 <- rep("Tolima", 120)
toli3<-cbind(tiempo,toli2,toli)
names(toli3)= c("tiempo","depar", "mes","infla","cmar")

################V. Cauca  
vall<- data.frame(ciclos[,c(1,49,98)])
vall2 <- rep("V. Cauca", 120)
vall3<-cbind(tiempo,vall2,vall)
names(vall3)= c("tiempo","depar", "mes","infla","cmar")


total<-rbind(anti3,atla3,bogo3,boli3,boya3,cald3,caqu3,cauc3,cesa3,cord3,cund3,choc3,huil3,lagua3,magd3,
              meta3,nari3,nort3,quin3,risa3,sant3,sucr3,toli3,vall3)

write.csv(total, file = "panelmeses2.csv")











################Todas las regiones 
#colombia<- data.frame(ciclos[,c(1,50,100)])
#colombia2 <- rep("Todas las regiones", 120)
#colombia3<-cbind(tiempo,colombia2,colombia)
#names(colombia3)= c("tiempo","depar", "mes","infla","cmar")


