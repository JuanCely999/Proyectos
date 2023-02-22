rm(list=ls())
library(rgdal) #readOGR
library(RColorBrewer)
library(classInt)
library(readxl)
dirmapas <- "/Users/asus/Desktop/Bibliografia/Precios rigidos/Econometria espacial/MGN2017_15_BOYACA/15_BOYACA"
setwd(dirmapas)
colombia <- readOGR("depto.shp",layer="depto")
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Bicentenario/Mapas en PDF/Figuras"
setwd(dir.resul)
plot(colombia)
colombia = colombia[colombia$DPTO !=88,]
View(colombia@data)

#Acronimos Dptos

AcrDpto <- c("ANT","ATL","BOG","BOL","BOY","CAL","CAQ","CAU","CES","COR","CUN","CHO","HUI","GUA","MAG","MET","NAR",
             "NOR","QUI","RIS","SAN","SUC","TOL","VALL","ARA","CAS","PUT","AMA","GUAI","GUAV","VAU","VICH")


centroides <- coordinates(colombia)
plot(colombia)
text(centroides,AcrDpto,cex=0.3)

Col <- read_excel("~/Clasificacion por tema/Trabajos/Bicentenario/MapasJhan/Col1.xlsx")
Col$Cod <- colombia@data$NOMBRE_DPT
names(Col)
LlarC <- as.matrix(Col[,4])
rownames(LlarC) <- AcrDpto
Visitantes <- Col[,4]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(colombia)
colombia.data <- SpatialPolygonsDataFrame(colombia, Visitantes)
plotvar <- colombia.data$Visitantes
nclr <- 9 # Numero de colores
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(round(plotvar,3)*100,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores


png("Colombia.png",width = 1200,height = 1800)
plot (colombia.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrDpto,cex=0.9)
dev.off()


