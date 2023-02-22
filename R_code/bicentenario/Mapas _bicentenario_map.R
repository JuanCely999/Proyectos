##############################################################################
###Ejercicio: Tratamiento de datos y visualizaciones en mapas#################
########Visistas guiadas: Proyecto 77 dias hacia la libertad################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#https://www.youtube.com/watch?v=YYh6D8x8qng&t=645s
########################################################
#       Estudiantes asistentes en epicentros           #
########################################################
rm(list=ls())
#install.packages("rgdal")
library(rgdal) #readOGR
#install.packages("readOGR")
#library(readOGR)
library(RColorBrewer)
#install.packages("classInt")
library(classInt)
library(readxl)
#####Tener cuidado con la ubicaci?n###########
dirmapas <- "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/Administrativo"
setwd(dirmapas)
boyaca <- readOGR("MGN_MPIO_POLITICO.shp",layer="MGN_MPIO_POLITICO")
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Bicentenario/Mapas en PDF/Figuras"
setwd(dir.resul)
#plot(boyaca)

#Acronimos Ciudades

AcrMpio <- c("TUN","ALM","AQU","ARC","BEL","BER","BET","BOA","BOY","BRI","BUE","BUS","CAL","CAM","CER","CHI","CHIQ",
             "CHI","CHIT","CHIV","CIE","COM","COP","COR","LAB","CAP","VIC","UVI","PES","PIS","PTO","QUI","SOC","SOCH",
             "SOG","SOM","UMB","VEN","VIR","ZET","COV","CUC","CUI","CHZ","CHV","DUI","COC","ESP","FIR","FLO","GAC","GAM",
             "GAR","GUA","GTQ","GYT","IZA","JEN","JER","VLL","MAC","MAR","MIR","MGA","MGI","MQR","MTV","MZO","NOB","NCO",
             "OIC","OTA","PAC","PAE","PPA","PAJ","PQB","PAU","PAY","PAZ","RAM","RAQ","RON","SAB","SAC","SAM","SED",
             "SJO","SLU","SMA","SMI","SPA","SNA","SMA","SRS","SSO","STN","STS","SIA","SOA","SRA","SOT","SCA","SSC",
             "STM","STT","TAS","TEN","TIB","TBS","TCA","TQE","TCA","TGU","TOP","TOT","TUN","TUR","TUT","TUZ","CHS",
             "CUB","GUI")


centroides <- coordinates(boyaca)
#plot(boyaca)
text(centroides,AcrMpio,cex=0.3)

#Col <- read_excel("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Bicentenario/MapasJhan/Col.xlsx")
Col <- read_excel("~/Documents/Investigacion/Bicentenario/MapaBicentenario/Bicentenario.xlsx")
Col$Cod <- boyaca@data$MPIO_CCDGO
names(Col)
LlarC <- as.matrix(Col[,18]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,18]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("EstEpicen.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()


#_________________________________________________________________________#
########################################################
#       Estudiantes asistentes en los diferentes municipios           #
########################################################
LlarC <- as.matrix(Col[,10]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,10]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("EstMun.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()

#_________________________________________________________________________#
########################################################
#       Porcentaje de las ciudades asistentes           #
########################################################
LlarC <- as.matrix(Col[,12]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,12]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 3 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*100,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("InsPor.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()


########################################################
#       Grupos por municipio           #
########################################################
LlarC <- as.matrix(Col[,9]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,9]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("GrupMun.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()








########################################################
#       EPICENTROS Y MUNICIPIOS         #
########################################################
LlarC <- as.matrix(Col[,5]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,5]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 31 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("epic2.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")), show.legend = FALSE,
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()











######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
LlarC <- as.matrix(Col[,20]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,20]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 31 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("epic2.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")), show.legend = FALSE,
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()






