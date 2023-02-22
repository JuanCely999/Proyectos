"
Fecha: 23/10/2019
Profesor: Eduard F. Martinez G.
Nota: No se usan acentos ni caracteres especiales para evitar conflictos al 
abrir este script en diferentes sistemas operativos
"

# Limpiar consola, limpiar memoria, fijar directorio y cargar paquetes
cat("\f")
rm(list = ls())
directorio <- "~/Documents/Investigacion/Mapas Pemp/Juan Pablo - Mapas/Clase 8/"
setwd(directorio)
paquetes <- c('dplyr','rgdal','rgeos','sf','sp','raster')
sapply(paquetes, require, character.only=T)
install.packages("sf")
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
mz_atlantico <- readOGR(dsn = "./datos/originales/08_atlantico/urbano/", layer = "MGN_URB_MANZANA")
length(mz_atlantico)

# Filtrando las manzanas de Barranquilla en la zona urbana
"Primero inspeccionemos el objeto espacial"
str(mz_atlantico@data)

"Convirtamos cada variables a sus valores numericos"
for (i in 1:ncol(mz_atlantico@data)){
     mz_atlantico@data[,i] <- mz_atlantico@data[,i] %>% as.character() %>% as.numeric()
}
str(mz_atlantico@data)

"Ahora filtremos los polygonos de Barranquilla"
table(mz_atlantico@data$CLAS_CCDGO)
mz_quilla <- mz_atlantico[mz_atlantico@data$MPIO_CCDGO==8001,]
length(mz_quilla)

"Dejemos unicamente las manzanas de la zona urbana"
table(mz_quilla@data$CLAS_CCDGO)
mz_quilla <- mz_quilla[mz_quilla@data$CLAS_CCDGO==1,]
plot(mz_quilla)

# Calculando los centroides de cada poligono
"(Explicar intuicion con circunferencia)"
cen_mz_quilla <- gCentroid(mz_quilla,byid = T)
plot(cen_mz_quilla , col="red",cex=0.1,pch=19)

"Pintando la capa de manzanas y la capa de centroides"
plot(mz_quilla,col="gray")
plot(cen_mz_quilla , col="red", cex=0.1 , add=T,pch=19)

"La nueva capa de putnos hereda los atributos de la capa de poligonos, asi como cada 
centroide hereda los atributos de cada manzana" 

#-------------------------------------#
# Verificando sistemas de coordenadas #
#-------------------------------------#
"Vamos a llevar todo a un sistema de coordenadas proyectadas, para efectos de 
las estimaciones"

# Debemos llevar todos los objetos a un mismo sistema de proyeccion 
"Veamos el tipo de proyeccion que tienen"
for ( i in c(cen_mz_quilla,mz_quilla,mz_atlantico)){
        print(proj4string(i))
}
"Reproyectemelos"
reproyectar <- function(Y){
               Y <- spTransform(Y,CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs'))
               return(Y)
}
cen_mz_quilla <- reproyectar(cen_mz_quilla)
mz_quilla <- reproyectar(mz_quilla)
mz_atlantico <- reproyectar(mz_atlantico)

"Veamos el tipo de proyeccion que tienen"
for ( i in c(cen_mz_quilla,mz_quilla,mz_atlantico)){
        print(proj4string(i))
}

#---------------------------#
# Cortar objetos espaciales #
#---------------------------#
"(Explicar con marcadores en el tablero)
(verificar algo)"

# Carguemos la capa de barrios, capa de restriccion y capa de transmetro
barrios <- readOGR(dsn ="./datos/originales/mapas/" ,layer = "Barranquilla Barrios")
barrios
barrios <- reproyectar(barrios)
barrios@data$id_barrio <- seq(1,length(barrios),1)
transmetro <- readOGR(dsn ="./datos/originales/mapas/" ,layer = "transmetro")
transmetro
transmetro <- reproyectar(transmetro)
cai <- readOGR(dsn ="./datos/originales/mapas/" ,layer = "estaciones_de_policia") %>% .[,c(2:4,6:7)]
cai
cai <- reproyectar(cai)

# Filtremos el barrio centro de Barranquilla
table(barrios@data$Nombre_Bar)
br_centro <- barrios[barrios@data$Nombre_Bar == "CENTRO",]
plot(barrios)
plot(br_centro,add=T,col="blue")
br_rosario <- barrios[barrios@data$Nombre_Bar == "EL ROSARIO",]
plot(br_rosario,add=T,col="pink")

# Funcion crop
"Deja todo los features de X que estan debajo de algun feature de Y"
plot(transmetro,col="red")
trans_rosario <- crop(x = transmetro,y = br_rosario)

"Veamos como se ve"
plot(br_rosario,col="gray")
plot(transmetro,col="red",add=TRUE)
plot(trans_rosario,add=TRUE,col="blue")

# Dejar los poligonos que se cruzan con vias 
"Deja todos los features de X que se cruzan con algun fetaure de Y"
trans_barrios <- intersect(x = barrios,y = transmetro)
plot(barrios)
plot(trans_barrios,col="pink",add=T)
plot(transmetro,col="blue",add=T)

# Dejar la diferencia entre dos objetos espaciles
plot(transmetro,col="red")
plot(br_rosario,add=T)
"Esta funcion elimina las partes del objeto spgeom1 que se cruzan con partes de el objeto spegeom2"
sin_trans_rosario <- gDifference(spgeom1 = transmetro,spgeom2 = br_rosario,byid = T)

"Veamos que hicimos"
plot(sin_trans_rosario,col="blue",add=T)

"Podemos mirar la diferencia entre dos poligonos"
plot(barrios,col="pink")
plot(trans_barrios,col="red",add=T)
sin_trans_barrios <- gDifference(spgeom1 = barrios,spgeom2 = trans_barrios)

"Veamos que hicimos"
plot(sin_trans_barrios,col="blue",add=TRUE)

# Usar un poligono para cortar una capa de puntos 
"Esta forma de cortar objetos funciona igual que la funcion intersect, es decir deja todos los 
elementos de la capa con los centroides de las manzanas que estan debajo de algun elemnto de 
la capa con el barrio centro"
cen_mz_centro <- cen_mz_quilla[br_centro,]

"Ploteando la capa del barrio centro y agregando los centroides de la capa de manzanas y la capa de manzanas"
plot(br_centro,col="pink")
plot(cen_mz_centro,col="blue",add=T,pch=19)
plot(mz_quilla,add=T)

# Usar un poligono para cortar una capa de poligonos 
"Aplica el mismo criterio que usamos anteriormente"
mz_centro <- mz_quilla[br_centro,]
"Fijenese que con esta opcion se dejan los objetos completos, es decir no los corta"
plot(br_centro,col="pink")
plot(mz_centro,col="royalblue",add=T)

#-------------------------------#
# Disolver entidades espaciales #
#-------------------------------#
"Veamos el mapa completo"
plot(readOGR(dsn = "./datos/originales/08_atlantico/MGN/", layer = "MGN_URB_SECTOR"),col="pink",border="red",lwd = 0.2)

"Veamos las variables"
str(readOGR(dsn = "./datos/originales/08_atlantico/MGN/", layer = "MGN_URB_SECTOR")@data)

"Carguemos unicamente las zonas urbanas de cada municipio"
comunas <- readOGR(dsn = "./datos/originales/08_atlantico/MGN/", layer = "MGN_URB_SECTOR") %>% 
           .[as.numeric(as.character(.@data$CLAS_CCDGO)) ==1,]

"Hagamos plot de las zonas urbanas sobre el mapa completo"
plot(comunas,col="gray",add=T,lwd = 0.2)  

"Convertir todos las entidades espaciales a una sola capa "
urbano_municipios <- gUnaryUnion(comunas,id = comunas$MPIO_CCDGO) %>% as(.,  "SpatialPolygonsDataFrame")
plot(urbano_municipios,col="royalblue",border="red")

"Otro ejemplo"
urbano_quilla <- comunas[as.numeric(as.character(comunas@data$MPIO_CCDGO))==8001,]
plot(urbano_quilla,col="pink",border="red",lwd = 0.5)
ciudad_quilla <- gUnaryUnion(urbano_quilla) %>% as(.,  "SpatialPolygonsDataFrame")
plot(ciudad_quilla,col="royalblue",border="red")

#---------------------------#
# Unir entidades espaciales #
#---------------------------#
"Veamos los mapas"
plot(br_centro,col="pink",border="red",lwd = 0.5)
plot(br_rosario,col="aquamarine",border="blue",lwd = 0.5,add=T)

"La funcion spRbind me permite unir objetos espaciales por filas (agregando observaciones)"
centro_rosales <- maptools::spRbind(br_centro,br_rosario)
plot(centro_rosales,col="pink",border="red",lwd = 0.5)

"Esta funcion se puede usar en un loop si se quiere, pero
se deben cambiar los IDs de los polygonos cuando estan repetidos"
"No tener en cuenta JP"
soledad <- readOGR(dsn = '~/data/original/maps/barranquilla/arcgis/',layer = 'soledad_habited')
barranquilla <- readOGR(dsn = '~/data/original/maps/barranquilla/arcgis/',layer = 'barranquilla_habited')
slot(slot(barranquilla,"polygons")[[1]], "ID") = "1"  # Se debe cambiar el ID del Polygono para hacer merge
city_polygon <- maptools::spRbind(barranquilla,soledad)

#------------------------------------------------------#
# Buffer y areas de influencia de una entidad espacial #
#------------------------------------------------------#
"Creamos una circuenferencia de 200 metros alrededor de cada CAI"
plot(cai,col="red",cex=0.4,pch=19)
buffer_cai <- gBuffer(spgeom = cai,width = 200)
plot(buffer_cai,add=T)

"Reducir el area de un poligono en 200 metros"
plot(ciudad_quilla,col="pink",border="red")
buffer_quilla <- gBuffer(spgeom = ciudad_quilla,width = -1000)

"!Debemos reproyectar no funciona con SC geograficas!"
ciudad_quilla <- reproyectar(ciudad_quilla)
buffer_quilla <- gBuffer(spgeom = ciudad_quilla,width = -1000)
plot(buffer_quilla,col="royalblue",border="blue",add=T)

#------------------------------------------------#
# Calcular distancias entre entidades espaciales #
#------------------------------------------------#

"Calculemos la distancia al borde de la restriccion"
restriccion <- readOGR(dsn = "./datos/originales/mapas/",layer = "restriction") %>% reproyectar(.)
cai$distancia1 <- gDistance(restriccion,cai, byid = TRUE)

"Calculemos la distancia al borde la zona sin restriccion"
plot(ciudad_quilla)
plot(restriccion,col="pink",add=T)
sin_restriccion <- gDifference(ciudad_quilla,restriccion)
plot(sin_restriccion,col="pink",border="red")
cai$distancia2 <- gDistance(sin_restriccion,cai, byid = TRUE)

#--------------------------------------------------------------------#
# Calcular el largo de un objeto tipo linea y el area de un poligono #
#--------------------------------------------------------------------#
"Para calcular el area de un poligono usamos la funcion gArea"
barrios@data$area_mt2 <- gArea(barrios, byid = TRUE)

"Para calcular el numero de kilometros de via por barrio"
plot(trans_barrios,col="pink")
plot(transmetro,col="red",add=T)

"Como lo vamos a hacer"
bar_i <- trans_barrios[1,]
plot(bar_i,col="blue",add=T)  
vias_i <- crop(transmetro,bar_i)
plot(vias_i,col="green",add=T)  
largo_i <- gLength(vias_i, byid = FALSE)

"Hagamos un loop"
id <- rep(NA,length(trans_barrios))
largo <- rep(NA,length(trans_barrios))
largo_vias <- cbind(id,largo) %>% as.data.frame()
for ( i  in 1:length(trans_barrios)){
         "seleccione el barrio i"
         bar_i <- trans_barrios[i,]
         id <- bar_i@data$id_barrio
         
         "Cortamos vias en el barrio i"
         vias_i <- crop(transmetro,bar_i)
         
         "Calculamos el largo de vias"
         largo_i <- gLength(vias_i, byid = FALSE)
         
         "Guardando informacion"
         largo_vias[i,1] <- id
         largo_vias[i,2] <- largo_i
}
largo_vias

barrios <- merge(barrios,largo_vias,by.x="id_barrio",by.y="id",all.x=T)
