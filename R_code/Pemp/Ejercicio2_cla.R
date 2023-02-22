"
Fecha: 30/10/2019
Profesor: Eduard F. Martinez G.
Nota: No se usan acentos ni caracteres especiales para evitar conflictos al 
abrir este script en diferentes sistemas operativos
"

# Limpiar consola, limpiar memoria, fijar directorio y cargar paquetes
cat("\f")
rm(list = ls())
directorio <- "~/Documents/Investigacion/Mapas Pemp/Juan Pablo - Mapas/Clase 9/"
#directorio <- "~/dropbox/teaching/taller de r/2019_2/clases/clase 9/"
setwd(directorio)
paquetes <- c('dplyr','rgdal','rgeos','sf','sp','raster','doBy')
sapply(paquetes, require, character.only=T)

"
1. Introduccion a datos raster
2. Cargar, inspeccionar, extraer informacion y guardar raster
3. Trabajar con rasters de varios layers
4. Operaciones con raster (cortar, medir distancias, calcular valores del raster para un poligono)
5. Rasterizar datos vectoriales (puntos o poligonos)
6. Aplicaciones con datos raster
7. Mostrar algunas fuentes de datos raster para Colombia y el mundo
"
#-------------------------------#
#  Introduccion a datos raster  #
#-------------------------------#

# Que es LIDAR
"LIDAR (Light Detection and Ranging o Laser Imaging Detection and Ranging) es 
es una tecnica de teledeteccion optica que utiliza la luz de laser para obtener
una muestra densa de la superficie de la tierra produciendo mediciones exactas
de x, y y z. Los productos de datos LiDAR se trabajan con mayor frecuencia en
un formato de datos de cuadricula o raster. Tomado de: ARCGIS"

# Que es un raster 
"
- Un archivo raster es una cuadricula regular de celdas, todas las cuales tienen el mismo 
tamano. 

- A cada celda se le llama pixel y cada pixel representa un area en el suelo.

- La resolucion del raster hace referecia al area que representa cada pixel en el suelo.  
"

# Como se ve un raster
"Imagen tomada de https://www.neonscience.org"
dev.off()
#plotRGB(stack("./datos/originales/raster.png"),r = 1, g = 2, b = 3)
plotRGB(stack("~/Documents/Investigacion/Mapas Pemp/Juan Pablo - Mapas/Clase 9/datos/originales/raster.png"),r = 1, g = 2, b = 3)

#--------------------------------------------------------------#
#  Cargar, inspeccionar, extraer informacion y guardar raster  #
#--------------------------------------------------------------#
# Carguemos un saptial polygon 
magdalena <- readOGR(dsn="./datos/originales/mgn_magdalena/",layer="MGN_Municipio")
magdalena

# Verifiquemos el objeto que vamos cargar"
GDALinfo("./datos/originales/siac/geotiff/DCCB_SMBYC_CBBQ_V5_1990_2000.tif")
"
- Nos muestra el numero de filas y columnas que tiene el raster
- Numero de Bandas
- La extension
- La resolucion de cada pixel
- Tipo de archivo
- La proyeccion
- Ubicacion del archivo
- Informacion de los pixelex 
"
# Cargando imagen de deforestacion"
deforestacion <- raster("./datos/originales/siac/geotiff/magdalena_deforestacion_1990_2000.tif")

# Inspeccionemos la informacion que contiene el raster"
deforestacion
"
- class: Nos indica el tipo de objeto espacial que estamos cargando

- dimensions: Nos muestra el numero de filas, columas y de celdas que tiene el raster

- resolution: Nos muestra la resolucion de cada pixel, es importante tener en cuenta el 
CRS que tiene el raster para saber en que unidades esta medida la resolucion

- extent: Nos muestra el bbox de el raster, es decir la extension que tiene la imagen, una
forma de verificar la resolucion de los pixeles es restando la extension (largo o ancho) del raster
y dividiendo por el numero de pixelex (filas o columnas).

- coord. ref: Nos muestra el CRS del raster

- data source: Ubicacion del archivo

- names: Nos muestra los nombres que tienen los layers o bandas
"

# Veamos los atributos del raster
"Nombres de las bandas"
names(deforestacion)

"Extension"
deforestacion@extent

"Proyeccion"
deforestacion@crs

"Valores minimos y maximos que contiene cada pixel (ver diccionario)"
minValue(deforestacion) ; maxValue(deforestacion)

# Como se ve el raster
"Hagamos un plot basico del raster"
plot(deforestacion)

"Agreguemos el spatialpolygon con los municipios del magdalena, pero primero reproyectemos el SpatialPolygon"
magdalena <- spTransform(magdalena, CRS(proj4string(deforestacion)))
plot(magdalena,border="blue",add=T)

# Juguemos un poco con los atributos del raster
"Renombremos los layers"
names(deforestacion) <- "cobertura_vegetal"
deforestacion

"Reproyectemos el raster (aca no usamos 'spTransform' porque es un raster no archivo de datos vectoriales)"
proj4string(deforestacion)
deforestacion_pr <- raster::projectRaster(deforestacion,crs = '+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs')
proj4string(deforestacion_pr)

# Como guardamos un archivo tipo raster
writeRaster(deforestacion_pr,filename = "./datos/procesados/magdalena_deforestacion_1990_2000.tif",overwrite=TRUE)

"Hay que tener cuidado, porque al reproyectar un raster pueden cambiar atributos dentro del raster
veamos que paso con los valores maximos y minimos que tenia cada grilla"
minValue(deforestacion_pr) ; maxValue(deforestacion_pr)

#----------------------------------------#
#  Trabajar con raster de varias bandas  #
#----------------------------------------#
# Fuente de estos raster
"Estos datos fueron tomados de: https://data.neonscience.org/apps/browse"

# Como se ve un RGB?
"Imagen tomada de https://www.neonscience.org"
dev.off()
plotRGB(stack("./datos/originales/rgb_raster.png"),r = 1, g = 2, b = 3)

# Verificando los atributos del archivo a cargar
GDALinfo("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

# Importando el archivo raster
"Como no le vamos a indicar que banda cargar, el va a cargar por 'default' la banda 1, es decir la roja"
RGB_band1_HARV <- raster(x = "./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

# Plot la banda 1
"Creemos una paleta de colores primero"
paleta_col <- gray.colors(n = 100, start = 0.0,end = 1.0,alpha = NULL) 

"Ahora si pintemos el raster usando la paleta de colores que definimos con anterioridad"
plot(RGB_band1_HARV,col=blues9,axes=FALSE)
plot(RGB_band1_HARV, col=paleta_col, axes=FALSE, main="Imagen RGB - Banda 1 (roja)") 

# Verifiquemos los valores minimos y maximos posibles de este layer 
"En un RGB podemos tener 255*255*255 posibles combinaciones, es decir 16.581.375 colores"
minValue(RGB_band1_HARV) ; maxValue(RGB_band1_HARV)
summary(RGB_band1_HARV)

# Importando una banda especifica (carguemos la banda de color verde)
"Para indicarle a R cual es la banda que quiero cargar, basta con 
agregar la opcion banda e indicarle el numero de la banda"
RGB_band2_HARV <- raster("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif", band = 2)
RGB_band2_HARV

# Ploteando la banda 2 (color verde)
plot(RGB_band2_HARV,col=blues9,axes=FALSE, main="Imagen RGB - Banda 2 (verde)") 

# Cargando las tres bandas de una sola vez
"Podemos cargar las 3 bandas de 2 formas  forma cargamos cada banda por individula y usamos
la opcion 'stack' para hacer un 'append' de las bases de datos o las cargamos de una vez con el comando stack"

"CArgando cada banda por aparte y apliando"
RGB_apilado <- stack(raster("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif",band=1),
                        raster("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif",band=2),
                        raster("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif",band=3))

"Cargando las tres bandas a la vez"
RGB_stack_HARV <- stack("./datos/originales/neon-ds-airborne-remote-sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

"Tambien podriamos usar la opcion brick que es mas eficiente en espacio"
object.size(RGB_stack_HARV)
RGB_brick_HARV <- brick(RGB_stack_HARV)
object.size(RGB_brick_HARV)

# Veamos los atributos de este objeto
"Atributos para todos los layers"
RGB_apilado
RGB_stack_HARV

"Atributos para cada hoja"
RGB_stack_HARV@layers
RGB_apilado@layers

"Atributos para la hoja 1"
RGB_apilado[[1]]

# Renombremos los nombres de los layers
names(RGB_apilado) <- c("red","green","blue")
names(RGB_stack_HARV) <- c("red","green","blue")

"Veamos los atributos nuevamente"
RGB_apilado
RGB_stack_HARV

# Remover layers 
"Con stack podemos apilar los layers de un raster, y con la opcion 'dropLayer'podemos 
eliminar los layers de un raster. Lo podemos hacer usando los nombres de los layers"
RGB_apilado_1 <- dropLayer(RGB_apilado, i = c("red","green"))
RGB_apilado_1
"O usando la posicion del layer"
RGB_apilado_2 <- dropLayer(RGB_apilado, i = c(1,3))
RGB_apilado_2

# Veamos como se dsitribuyen la intensidad de colores dentro de cada layer
"Noten que cuando usamos las funciones plot o hist sin indicarle el layer, 
R hace un plot o un hsitograma para cada layer"
hist(RGB_stack_HARV, maxpixels=ncell(RGB_stack_HARV),col="gray")

# Vamos a plotear todos los layers que tiene el raster
"Usamos la funcion plot, al igual que con el histograma nos va a mostrar un plot por layer"
plot(RGB_stack_HARV, col=blues9)

"Si queremos plotear un solo layer le indicamos a R el layer a plotear"
plot(RGB_stack_HARV[[2]],col=blues9,axes=FALSE, main="Imagen RGB - Banda 2 (verde)") 

# Plotear el RGB completo
"Para plotear el RGB usamos la funcion plotRGB"
plotRGB(RGB_stack_HARV, r = 1, g = 2, b = 3)

"Es importante entender el orden de los layers, porque Si alteramos el orden de las capas se altera
los colores en la imagen que se plotea"
plotRGB(RGB_stack_HARV, r = 1,g=3,b=2)

"Si no indicamos el color de cada laer, plorRBG asume que el layer 1 es rojo, el 2 es verde y 3 es azul"
plotRGB(RGB_stack_HARV)

"Los apilados usando brick tambien se plotean con plotRGB o plot"
plotRGB(RGB_brick_HARV)
plot(RGB_brick_HARV)

#----------------------------------------------------------------------------------------#
#  Operaciones con raster (cortar, medir distancias, calcular valores para un poligono)  #
#----------------------------------------------------------------------------------------#

# Veamos que tenemos 
magdalena
plot(magdalena)
deforestacion@crs

# Deforestacion en Concordia (funcion crop)
"Primero filtremos el poligono de Santa Marta"
magdalena$MPIO_CCDGO <- as.character(magdalena$MPIO_CCDGO) %>% as.numeric()
concordia <- magdalena[magdalena$MPIO_CNMBR == "CONCORDIA",]

"Usemos la funcion crop para cortar el raster, nuevamente el primer argumento es el objeto
al que le queremos hacer el corte y el segundo argumento es el molde que vamos a usar"
plot(concordia)
concordia_raster <- crop(deforestacion,concordia)
plot(concordia_raster)
plot(concordia,add=T,border="black")

"Observese que aqui la funcion crop no funciona como cuando cortamos poligonos, es decir
no corta el raster sino que deja unicamente un raster con la extention de el poligono "
concordia@bbox
concordia_raster@extent

"Entonces para extraer informacion de un raster, primero vamos a calcular los centroides de cada pixel"
point_concordia_raster <- rasterToPoints(concordia_raster, spatial=TRUE)

"Obtenemos esto"
point_concordia_raster
plot(point_concordia_raster, col="green",cex=0.05,pch=19)
plot(concordia, border="blue",pch=19,add=TRUE)

"Ahora vamos a quedarnos unicamente con los centroides que estan debajo de concordia"
point_concordia_raster <- point_concordia_raster[concordia,]
plot(point_concordia_raster, col="pink",cex=0.07,pch=19)
plot(concordia, border="blue",pch=19,add=TRUE)

"Ahora extraemos un datframe que contiene la informacion de cada pixel que esta debajo de concordia"
df_deforest_concordia <- point_concordia_raster %>% as.data.frame(.)

# Funcion para agregar a cada municipio la deforestacion 
fucntion_deforestacion <- function(municipio){
                          polygono <- magdalena[magdalena@data$MPIO_CNMBR == municipio,] 
                          polygono_crop <- crop(deforestacion,polygono)
                          centroid <- rasterToPoints(polygono_crop, spatial=TRUE) 
                          centroid <- centroid[polygono,]
                          point_data <- centroid@data
                          point_data$conteo <- 1
                          collapse1 <- summaryBy(conteo ~ cobertura_vegetal , FUN=c(sum), data=point_data)
                          collapse1$MPIO_CNMBR <- municipio
return(collapse1)
}

"Veamos un ejemplo"
fucntion_deforestacion("PEDRAZA")

"Intenten hacer esto en loop para todos los municipios del Magdalena"

# Extrayendo informacion para cada municipio del magdalena (no correr esto en clase, puede tardar mucho)
"Otra forma de hacerlo es usando la funcion extrac, primero metemos el raster al que le vamos a
extraer la informacion, despues el spatialpolygon o spatialpoint que usaremos para extraer la 
informacion del raster"
df_defo_santa <- extract(deforestacion,magdalena,df=TRUE,id="MPIO_CCDGO")

# Calcular la distancia del centroide de cada pixel a el centro urbano de Concordia
"Vamos a calcular la distancia de cada pixel al centro urbano, usando las funciones vistas 
en la clase anterior. Primero vamos a construir 'el centro urbano' de el municipio, asi que 
vamos a reproyectar la capa de el municipio de concordia, antes de calcula el centroide y el buffer"
point_concordia_raster <- spTransform(point_concordia_raster,CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs'))
concordia <- spTransform(concordia,CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs'))
centro_urbano <- gCentroid(concordia) %>% gBuffer(.,width = 1000)

"Veamos que hicimos"
plot(point_concordia_raster,col="pink",cex=0.05,pch=19)
plot(concordia,border="blue",add=TRUE)
plot(centro_urbano,border="red",add=TRUE)

"Calculemos la distancia"
point_concordia_raster@data$distancia <- gDistance(centro_urbano,point_concordia_raster,byid = TRUE)

"Veamos que tenemos"
head(point_concordia_raster@data)

"Cual es la disatncia promedio (piensen que pueden hacer esto con carreteras por ejemplo)"
summaryBy(distancia ~ cobertura_vegetal , FUN=c(mean), data=point_concordia_raster@data)

# Convertir un polygono en un raster
"Primero tenemos que conocer las dimensiones del polygono (puntos maximos y minimos de las coordenadas)"
concordia@bbox

"Creemos el raste"
base_raster <- raster(ncols = 100, nrows = 100, 
                      xmn = concordia@bbox[1,1] , xmx = concordia@bbox[1,2],
                      ymn = concordia@bbox[2,1] , ymx = concordia@bbox[2,2]) 
"Asignemosle un sistema de coordenadas"
proj4string(base_raster) <- CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs')
base_raster

"Convirtamos el municipio en raster, primero va el polygono y despues el raster"
raster_concordia <- rasterize(concordia,base_raster) 
raster_concordia
plot(raster_concordia,col="gray")
plot(concordia,border="blue",add=TRUE)

#-----------------------------------#
#  Aplicaciones con datos de luces  #
#-----------------------------------#

# CAlculemos la correlacion entre luminosidad promedio y IIM
"Carguemos los datos de luces"
luces <- raster("./datos/originales/visible-earth-nasa/BlackMarble_2016_3km_gray_geo.tiff")
luces

"Definamos una paleta de colores"
plot(luces,col=blues9,axes=FALSE) 

"Dejemos unicamente las luces para el Magdalena"
magdalena <- spTransform(magdalena, CRS(proj4string(luces)))
luces_magdalena <- crop(luces,magdalena)
plot(luces_magdalena,col=blues9,axes=FALSE) 
plot(magdalena,border="red",add=TRUE,axes=FALSE) 

"Calculemos los centroides"
point_luces <- rasterToPoints(luces_magdalena, spatial=TRUE)

"Terminar ejemplo en clases"
id <- rep(NA,length(magdalena))
promedio <- rep(NA,length(magdalena))
df_promedio <- cbind(id,promedio) %>% as.data.frame()

"Loop"
for ( i  in 1:length(magdalena)){
      polygono <- magdalena[i,] 
      centroid <- point_luces[polygono,]
      point_data <- centroid@data
      id <- polygono@data$MPIO_CCDGO %>% as.character(.)
      
      collapse1 <- summaryBy(BlackMarble_2016_3km_gray_geo ~ MPIO_CCDGO, FUN=c(mean), data=point_data)
      collapse1$MPIO_CCDGO <- id
      "Guardando informacion"
      df_promedio[i,1] <- id
      df_promedio[i,2] <- collapse1
}
df_promedio

"Cargemos datos de IIM"
iim <- readxl::read_excel("./datos/originales/dane/imm.xlsx")
iim$cod_muni <- as.numeric(iim$cod_muni) - 47000

"Hagamos el merge"
municipios <- merge(iim,df_promedio,by.x="cod_muni",by.y="id",all.x=T)
plot(municipios$PIB ,municipios$promedio,axes=F)

#------------------------------------------#
#  Fuentes de informacion de datos raster  #
#------------------------------------------#

# Colombia deforestacion
"http://www.siac.gov.co"

# Luces de 1992-2013 (anuales)
"https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html"

# Luces de 2012-2019 (mensuales)
"https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html"

# luces devtools::install_github("chrisvwn/Rnightlights") 
"https://github.com/chrisvwn/Rnightlights" 

# Proyecto NEON
link <- "https://data.neonscience.org/apps/browse"

" El observatorio NEON esta diseñado para recopilar datos estandarizados de alta
calidad de 81 sitios de campo (47 terrestres y 34 acuaticos) en los Estados Unidos
(incluidos Alaska, Hawai y Puerto Rico)."

# Otras fuentes
"https://developers.google.com/earth-engine/datasets/catalog"

# ONY
"http://biesimci.org/Satelital/Auxiliar/Basesimci/Indices/Basesimci.html"
