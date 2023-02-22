##############################################################################
#     Ejercicio: Tratamiento de datos y visualizaciones en mapas, terridata  #
#                             Demografia 2019 y 1985                         #
#                              Autor: Juan Pablo Cely                        #
#                                    19-03-2020                              #
##############################################################################
#Codigos
#15293 GACHANTIV? Gachantiv?
#15808 TINJAC? Tinjac?
#15407 VILLA DE LEYVA Villa De Leyva
#15776 SUTAMARCH?N Sutamarch?n
#15696 SANTA SOF?A Santa Sof?a
#15600 R?QUIRA R?quira
#15638 S?CHICA S?chica


############Ubicacion municipio de alto ricaurte
#install.packages(c("classInt", "RColorBrewer", "readxl", "rgdal"))
rm(list=ls())
library(rgdal) #readOGR
#install.packages("readOGR")
#library(readOGR)
library(RColorBrewer)
#install.packages("classInt")
library(classInt)
library(readxl)
#####Tener cuidado con la ubicaci?n###########
###Filtrar manual excel
dirmapas <- "/Users/juanpablo/Documents/Modelos/Precios rigidos/Econometria espacial/MGN2017_15_BOYACA/15_BOYACA/Administrativo"
setwd(dirmapas)
boyaca <- readOGR("MGN_MPIO_POLITICO.shp",layer="MGN_MPIO_POLITICO")
dir.resul<-"/Users/juanpablo/Documents/Alto ricaurte/Analisis alto ricaurte/Figuras"
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
"~/Documents/Libros.xlsx"
Col <- read_excel("~/Clasificacion por tema/Trabajos/Alto ricaurte/Analisis alto ricaurte/agregado.xlsx")
#Col<- filter(Col, codi== "15") 
Col$Cod <- boyaca@data$MPIO_CCDGO

LlarC <- as.matrix(Col[,6]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,6]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,1)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("agregado4.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()


########################################
########################################
LlarC <- as.matrix(Col[,8]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,8]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 8 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,1)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("alto2.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()
#######################################################
#######################################################
#######################################################

# NBI DIM14
NBI <- read_excel("~/Clasificacion por tema/Trabajos/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
names(NBI)
#CONVERTIR DATOS EN NUMERO
NBIDEP<- filter(NBI, `C?digo Departamento`==15 )

NBIDEP<- filter(NBIDEP, `C?digo Departamento`==15 )
NBIDEP<- filter(NBIDEP, `C?digo Entidad`!=15000 )
NBIURB<- filter(NBIDEP, Indicador== "?ndice de Necesidades B?sicas Insatisfechas - NBI - en el ?rea urbana" )

NBIRUR<- filter(NBIDEP, Indicador== "?ndice de Necesidades B?sicas Insatisfechas - NBI - en el ?rea rural")
view(NBIRUR)
NBITOTAL<- cbind(NBIURB,NBIRUR)
NBIexp<- data.frame(NBITOTAL[,c(1:4,7:8,21)])
setwd("~/Clasificacion por tema/Trabajos/Alto ricaurte/Analisis alto ricaurte")
write.csv(NBIexp, file = "NBI.csv")
#NBI2=merge en excel con merge de municipios NBI
#NBI3= A?ADE ALTO RICAURTE
#CORRER LA PRIMERA PARTE
Col <- read_excel("~/Clasificacion por tema/Trabajos/Alto ricaurte/Analisis alto ricaurte/NBI3.xlsx")
########################################
########################################NBI URBANO
Col$Cod <- boyaca@data$MPIO_CCDGO
LlarC <- as.matrix(Col[,6]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,6]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,1)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores
?classIntervals
png("NBIURBANO.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()

########################################
########################################NBI URBANO
LlarC <- as.matrix(Col[,7]) ###Numero de la columnna de la variable
rownames(LlarC) <- AcrMpio
Visitantes <- Col[,7]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(boyaca)
boyaca.data <- SpatialPolygonsDataFrame(boyaca, Visitantes)
plotvar <- boyaca.data$Visitantes
nclr <- 5 # Numero de colores 8 para solo la zona de ricaurte alto
plotclr <- brewer.pal(nclr,"Oranges")
class <- classIntervals(round(plotvar,1)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores

png("NBIRURAL.png",width = 1800,height = 1200)
plot (boyaca.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrMpio,cex=0.9)
dev.off()

