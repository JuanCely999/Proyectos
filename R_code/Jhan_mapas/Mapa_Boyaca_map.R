rm(list=ls())
#install.packages("rgdal")
library(rgdal) #readOGR
#install.packages("readOGR")
#library(readOGR)
library(RColorBrewer)
#install.packages("classInt")
library(classInt)
library(readxl)
#####Tener cuidado con la ubicación###########
dirmapas <- "/Users/asus/Desktop/Bibliografia/Precios rigidos/Econometria espacial/MGN2017_15_BOYACA/15_BOYACA/Administrativo"
setwd(dirmapas)
boyaca <- readOGR("MGN_MPIO_POLITICO.shp",layer="MGN_MPIO_POLITICO")
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Bicentenario/MapasJhan"
setwd(dir.resul)
plot(boyaca)

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
plot(boyaca)
text(centroides,AcrMpio,cex=0.3)

#Col <- read_excel("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Bicentenario/MapasJhan/Col.xlsx")
Col <- read_excel("Col2.xlsx")

Col$Cod <- boyaca@data$MPIO_CCDGO
names(Col)
LlarC <- as.matrix(Col[,4])
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,4]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 9 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,3)*100,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("PropIndM3.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()










