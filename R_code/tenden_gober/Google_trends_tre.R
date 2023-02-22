##############################################################################
###Proyecto colciencias: Turismo y gobernanza. Tendencias en Google##########
#######################Graficas de mapa y series de tiempo###################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################

#Google con Google trends pero con los mapas basicos
#Codigo base 
# http://www.diegocalvo.es/obtener-datos-de-google-trends-con-r-usando-su-api/

#Importante para la edici?n
#https://www.rdocumentation.org/packages/rworldmap/versions/1.3-6/topics/mapCountryData

#Proposito del codigo
#Mapear el mundo y la frecuencia desde el 2005- turismo y gobernanza
#Mapear Colombia y la frecuencia desde el 2005- turismo y gobernanza
#No hacer wordclouds, mejor en el momento de redacci?n 

rm(list=ls())
library(gtrendsR) 
library(reshape2)
library(maps)
library(rworldmap)
#CO = COLOMBIIA, US = USA


###################################################
# Tendencias en el mundo sobre gobernanza y turismo#
####################################################
cat("\f")
rm(list = ls())
#turismo
global_trends <- gtrends(c("tourism"), gprop = "web", time = "2005-01-01 2019-12-01")
global_trends_by_country <- data.frame(country=global_trends$interest_by_country[,1], value=global_trends$interest_by_country[,2])
write.csv(global_trends_by_country, file = "global_trends_by_country.csv")

global_trends_by_country <- read_csv("global_trends_by_country.csv")
global_trends_by_country$value<-as.numeric(global_trends_by_country$value)

spdf <- joinCountryData2Map(global_trends_by_country, joinCode="NAME", nameJoinColumn="country")
#win.graph()
mapCountryData(spdf, nameColumnToPlot="value",catMethod="fixedWidth", addLegend = TRUE, mapTitle = "") 







op <- palette(c("lavender", "yellow", "orange", "red"))

cutVector <- c(min(global_trends_by_country$value),0,20,40,60,80,100, max(global_trends_by_country$value))
# Define the categories and name them for the legend
spdf@data[["value"]] <- cut(spdf@data[["value"]], cutVector, include.lowest=TRUE)
levels(spdf@data[["value"]]) <- c("0,20", "21,40","41,60","61,80","81,100")
#   Plot the map spdf, use the categories defined with categories, the palette previously defined and a legend.
par(mai=c(0,0,0.1,0),xaxs="i",yaxs="i")
mapParams <- mapCountryData(spdf, nameColumnToPlot="value", colourPalette = "palette", addLegend = T, missingCountryCol = NA,
                            catMethod = "categorical", mapTitle = "")
# At end of plotting, reset palette to previous settings:
palette(op)













cat("\f")
rm(list = ls())
#gobernanza#
global_trends <- gtrends(c("governance"), gprop = "web", time = "2005-01-01 2019-10-30")
global_trends_by_country <- data.frame(country=global_trends$interest_by_country[,1], value=global_trends$interest_by_country[,2])

write.csv(global_trends_by_country, file = "global_trends_by_country2.csv")

global_trends_by_country <- read_csv("global_trends_by_country2.csv")
global_trends_by_country$value<-as.numeric(global_trends_by_country$value)
spdf <- joinCountryData2Map(global_trends_by_country, joinCode="NAME", nameJoinColumn="country")
#win.graph()
mapCountryData(spdf, nameColumnToPlot="value",  catMethod = "pretty", colourPalette = "heat", mapTitle = "") 
#Nota: extraer las palabras claves 




global_trends_by_country$value<-as.numeric(global_trends_by_country$value)



op <- palette(c("lavender", "yellow", "orange", "red"))

cutVector <- c(min(global_trends_by_country$value),0,20,40,60,80,100, max(global_trends_by_country$value))
# Define the categories and name them for the legend
spdf@data[["value"]] <- cut(spdf@data[["value"]], cutVector, include.lowest=TRUE)
levels(spdf@data[["value"]]) <- c("0,20", "21,40","41,60","61,80","81,100")
#   Plot the map spdf, use the categories defined with categories, the palette previously defined and a legend.
par(mai=c(0,0,0.1,0),xaxs="i",yaxs="i")
mapParams <- mapCountryData(spdf, nameColumnToPlot="value", colourPalette = "palette", addLegend = T, missingCountryCol = NA,
                            catMethod = "categorical", mapTitle = "")
# At end of plotting, reset palette to previous settings:
palette(op)






library(ggrepel)
library(tidyverse)
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}
#http://rpubs.com/utjimmyx/429204
#Todo el mundo en turismo
es_trends1 <- gtrends(c("tourism"), gprop = "web", time = "2005-01-01 2019-12-01")
#es_trends_interest_over_time <- es_trends$interest_over_time[,1:2]
#plot(es_trends_interest_over_time, type="l", col="blue")
plot(es_trends1)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra
es_trends1$interest_by_country[,1:2]
###Temas  del  turismo en el mundo
View(es_trends1$related_topics[,3])
View(es_trends1$related_queries[,3])







#Gobernanza
es_trends11 <- gtrends(c("governance"), gprop = "web", time = "2005-01-01 2019-12-01")
#es_trends_interest_over_time <- es_trends$interest_over_time[,1:2]
#plot(es_trends_interest_over_time, type="l", col="blue")
plot(es_trends11)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra
es_trends11$interest_by_country[,1:2]
###Temas  de gobernanza en el mundo
View(es_trends11$related_topics[,3])
View(es_trends11$related_queries[,3])
#############################################



#Tendencia regional para Colombia  en turismo("tourism","governance") En ingles
es_trends2 <- gtrends(c("turismo"), geo = c("CO"), gprop = "web", time = "2005-01-01 2019-12-01")
plot(es_trends2)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra

#turismo
View(es_trends2$interest_by_region[,1:2])
####Tema de turismo en colombia
es_trends2$interest_by_city[,1:2]
es_trends2$related_topics[,3]
es_trends2$related_queries[,3]




#Tendencia regional para Colombia  en gobernanza
es_trends22 <- gtrends(c("gobernanza"), geo = c("CO"), gprop = "web", time = "2005-01-01 2019-12-01")
plot(es_trends22)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra
#Gobernanza
es_trends22$interest_by_region[,1:2]
###Temas  de  gobernanza en Colombia
es_trends22$interest_by_city[,1:2]
es_trends22$related_topics[,3]
es_trends22$related_queries[,3]
####Mapear por aparte el mapa de Colombia ####VER AL FINAL
#########################################################



##################################################





#################MAPA DE COLOMBIA EN INTERES POR TURISMO
############################################
rm(list=ls())
library(rgdal) #readOGR
library(RColorBrewer)
library(classInt)
library(readxl)
#install.packages("ggplot")
library(ggplot2)
dirmapas <- "/Users/asus/Desktop/Bibliografia/Precios rigidos/Econometria espacial/MGN2017_15_BOYACA/15_BOYACA"
setwd(dirmapas)
colombia <- readOGR("depto.shp",layer="depto")
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/informe/Figuras"
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

Col <- read_excel("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Col1.xlsx")
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
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores


png("turismo.png",width = 1200,height = 1800)
plot (colombia.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrDpto,cex=0.9)
dev.off()









#################MAPA DE COLOMBIA EN INTERES POR GOBERNANZA
############################################
rm(list=ls())
library(rgdal) #readOGR
library(RColorBrewer)
library(classInt)
library(readxl)
#install.packages("ggplot")
library(ggplot2)
dirmapas <- "/Users/asus/Desktop/Bibliografia/Precios rigidos/Econometria espacial/MGN2017_15_BOYACA/15_BOYACA"
setwd(dirmapas)
colombia <- readOGR("depto.shp",layer="depto")
dir.resul <- "C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/informe/Figuras"
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

Col <- read_excel("~/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Col1.xlsx")
Col$Cod <- colombia@data$NOMBRE_DPT
names(Col)
LlarC <- as.matrix(Col[,6])
rownames(LlarC) <- AcrDpto
Visitantes <- Col[,6]
Visitantes <- as.data.frame(Visitantes) 
names(Visitantes)<-"Visitantes"
row.names(Visitantes) <- row.names(colombia)
colombia.data <- SpatialPolygonsDataFrame(colombia, Visitantes)
plotvar <- colombia.data$Visitantes
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(round(plotvar,3)*1,nclr) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores


png("gobernanza.png",width = 1200,height = 1800)
plot (colombia.data, col=colcode, border="grey", axes=T)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=2.5)
text(centroides,AcrDpto,cex=0.9)
dev.off()

#######################################################################
#Geolocalizacion
#######################################################################
install.packages("ggmap")
library(ggmap)

#register_google(key="AIzaSyDhYSCEscgQ_KE6iRqu80-XnKjBsG973WM")
register_google(key = "AIzaSyB0mzVmrjCnJUhf5EvLvmaV997fOoGyVEY")#Mi API KEY
geocode("Berlin")
