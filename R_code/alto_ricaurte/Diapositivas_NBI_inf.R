###############Ejercicio: Mapas Municipios del Alto Ricaurte#################
##############################################
######################Autor: Juan Pablo Cely#################################
###############################29-03-2020####################################
cat("\f")
rm(list = ls())
directorio <- "~/Documents/Investigacion/Mapas Pemp/"
setwd(directorio)
paquetes <- c('dplyr','rgdal','rgeos','sf','sp','raster')
sapply(paquetes, require, character.only=T)
library(dplyr)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
library(raster)

"
1. Calcular centroides
2. Opciones para cortar entidades espaciales
3. Disolver polygonos
4. Unir entidades espaciales
5. Buffer y areas de influencia de una entidad espacial
6. Calcular distancias entre entidades espaciales
7. Calcular el largo de un objeto tipo linea y el area de un poligono

(Breve explicacion de la base de datos MGN DANE)"

#---------------------#
# Calcular centroides #
#---------------------#
boyaca<-readOGR(dsn = "./15_BOYACA/Administrativo/", layer = "MGN_MPIO_POLITICO")
gachantiva <- boyaca[boyaca@data$MPIO_CCDGO==15293,]
plot(gachantiva)

mz_boyaca <- readOGR(dsn = "./15_BOYACA/URBANO/", layer = "MGN_URB_MANZANA")
mz_boyaca2 <- readOGR(dsn = "./15_BOYACA/MGN/", layer = "MGN_RUR_SECCION")

length(mz_boyaca)

# Filtrando las manzanas de Barranquilla en la zona urbana
"Primero inspeccionemos el objeto espacial"
str(mz_boyaca@data)

"Convirtamos cada variables a sus valores numericos"
for (i in 1:ncol(mz_boyaca@data)){
  mz_boyaca@data[,i] <- mz_boyaca@data[,i] %>% as.character() %>% as.numeric()
}
str(mz_boyaca@data)

"Ahora filtremos los polygonos de Tunja"
table(mz_boyaca@data$CLAS_CCDGO)
mz_gachantiva <- mz_boyaca[mz_boyaca@data$MPIO_CCDGO==15293,]
length(mz_gachantiva)
plot(mz_gachantiva)
view(mz_gachantiva@data)




length(mz_boyaca2)

# Filtrando las manzanas de Barranquilla en la zona urbana
"Primero inspeccionemos el objeto espacial"
str(mz_boyaca2@data)

"Convirtamos cada variables a sus valores numericos"
for (i in 1:ncol(mz_boyaca2@data)){
  mz_boyaca2@data[,i] <- mz_boyaca2@data[,i] %>% as.character() %>% as.numeric()
}
str(mz_boyaca2@data)

"Ahora filtremos los polygonos de Tunja"
table(mz_boyaca2@data$CLAS_CCDGO)
rur_gachantiva <- mz_boyaca2[mz_boyaca2@data$MPIO_CCDGO==293,]
length(rur_gachantiva)


plot(gachantiva)
plot(rur_gachantiva,col="blue",pch=19,add=T)
plot(mz_gachantiva,col="pink",add=T)


library(haven)
vivienda <- read_dta("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/15_Boyaca/15_Boyaca/15_Boyaca_DTA/CNPV2018_1VIV_A2_15.DTA")
vivi_gac<- filter(vivienda, `u_mpio`==293 )

georeferencia <- read_dta("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/15_Boyaca/15_Boyaca/15_Boyaca_DTA/CNPV2018_MGN_A2_15.DTA")
georef<- filter(georeferencia, `u_mpio`==293 )
names(manz_boy)
gach<- filter(georef, `u_mpio`==293 )
view(gach)
estratos<-merge(x = vivi_gac, y = gach, by = "cod_encuestas", all = TRUE)
view(estratos)
names(estratos)
table(estratos$u_mza, estratos$va1_estrato)

#        Copiar a excel y modificar desde excel               #
#Merge con el mapa de manzanas
library(readxl)
Estratos <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Georeferencias/Estratos.xlsx")
Estratos$Manzana <- mz_gachantiva@data$MANZ_CCDGO



Col<-merge(x = depart, y = visitas, by = "Cod", all = TRUE)



tipo <- c("A","B","C","D","E","F","G","H")

set.seed(1234)
valor.x <- rnorm(8,25,10)
set.seed(4321)
valor.y <- rnorm(8,25,10)
tamano <- rnorm(8,20,10)
set.seed(1234)
color <- c(rep("blue",2), rep("red",2),rep("orange",2),rep("yellow",2))

datos <- data.frame(tipo, valor.x, valor.y, tamano, color)
view(datos)
library(ggplot2)

ggplot(datos, aes(x = valor.x, y = valor.y, label = tipo)) +
  geom_point(aes(size = tamano, colour = color)) + 
  geom_text(hjust = 1, size = 2) +
  theme_minimal()
