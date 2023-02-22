##############################################################################
###Ejercicio: Tratamiento de datos y visualizaciones en mapas#################
########Visistas turisticas nacionales################
######################Autor: Juan Pablo Cely#################################
###############################29-03-2020####################################
#mapa basico
rm(list=ls())
library(rgdal) #readOGR
library(RColorBrewer)
library(classInt)
library(readxl)
install.packages("ggplot")
library(ggplot2)
#dirmapas <- "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/Colombia"
dirmapas <- "~/Documents/Investigacion/Mapas Pemp/15_BOYACA"
setwd(dirmapas)
colombia <- readOGR("depto.shp",layer="depto")
#colombia <- readOGR("COLOMBIA.shp",layer="COLOMBIA")
dir.resul <- "/Users/juanpablo/Documents/Tendencias mundiales. Gobernanza/informe/Figuras"
setwd(dir.resul)
plot(colombia)
colombia = colombia[colombia$DPTO !=88,]
#View(colombia@data)

#Acronimos Dptos

AcrDpto <- c("ANT","ATL","BOG","BOL","BOY","CAL","CAQ","CAU","CES","COR","CUN","CHO","HUI","GUA","MAG","MET","NAR",
             "NOR","QUI","RIS","SAN","SUC","TOL","VALL","ARA","CAS","PUT","AMA","GUAI","GUAV","VAU","VICH")


centroides <- coordinates(colombia)
plot(colombia)
text(centroides,AcrDpto,cex=0.3)


####Merge entre la principal base de datos y los codigos
depart <- read_excel("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Turismo Dane/depart.xlsx")
visitas <- read_excel("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Turismo Dane/visitas.xlsx")

Col<-merge(x = depart, y = visitas, by = "Cod", all = TRUE)
Col$Cod <- colombia@data$NOMBRE_DPT
names(Col)
LlarC <- as.matrix(Col[,5])
rownames(LlarC) <- AcrDpto
Visitantes <- Col[,5]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(colombia)
colombia.data <- SpatialPolygonsDataFrame(colombia, Visitantes)
plotvar <- colombia.data$Visitantes
nclr <- 9 # Numero de colores
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores


png("Colombia.png",width = 1200,height = 1800)
plot (colombia.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrDpto,cex=0.9)
dev.off()

#tener en cuenta para mapas de burbujas en Colombia