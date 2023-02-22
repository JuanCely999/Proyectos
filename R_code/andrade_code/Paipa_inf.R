##############################################################################
###Diagnostico para Paipa- Juan Pablo Andrade################################
######################Autor: Juan Pablo Cely#################################
###############################25-07-2020####################################
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
library(reshape2)

library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)


library(gtrendsR) 
library(reshape2)
library(maps)
library(rworldmap)
library(rgdal) 
library(wordcloud)
library(tm)
cat("\f")
rm(list = ls())

 
colombia <-  st_read(dsn = "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/", layer = "depto")
setwd("~/Documents/Investigacion/Juan Pablo Andrade/Turismo en Paipa/Estadisticas/")

paipaR <- read_excel(paste("paipaR.xlsx",sep=""),2)
names(paipaR)

#colo3<-rename(colo3,Porcentaje=n)
colo3<- paipaR %>% mutate(totH = sum(per)) %>%
  mutate(Porcentaje = (per/totH)*100) %>% mutate(totH2 = sum(viaje)) %>%
  mutate(Porcentaje_ = (viaje/totH2)*100) 


colombiafab <- colombia %>% 
  left_join(colo3)

#colombiafab[is.na(colombiafab)] <- 0

colombiafabric <- colombiafab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                          st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                             2))
colombiafab <- cbind(colombiafab, st_coordinates(st_centroid(colombiafab$geometry)))#importante
#Per capita dia turismo interno
ggplot(colombiafab)+ # tamaño 1000*800
  geom_sf(aes(fill=per))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = Porcentaje),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Per cápita por dia (miles) ", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 10), 
                        breaks = c(2.5,3,3.5,4,4.5,5,5.5,6))+ # nombres en colors()
  geom_text_repel(colombiafabric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(colombiafabric), datum = NA)+
  theme_void()
summary(colombiafab$Porcentaje) 

#

#Por viaje turismo interno
ggplot(colombiafab)+ # tamaño 1000*800
  geom_sf(aes(fill=viaje))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = Porcentaje_),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Por viaje (miles)", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 20), 
                        breaks = c(2.5,3,3.5,4,4.5,5,5.5,6))+ # nombres en colors()
  geom_text_repel(colombiafabric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(colombiafabric), datum = NA)+
  theme_void()

summary(colombiafab$Porcentaje_)






##########Valor agregado PIB Boyaca#####################
#####################################
cat("\f")
rm(list = ls())


boyaca_mapa <-  st_read(dsn = "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/ADMINISTRATIVO/", layer = "MGN_MPIO_POLITICO")
setwd("~/Documents/Investigacion/Juan Pablo Andrade/Turismo en Paipa/Estadisticas/")

paipaR <- read_excel(paste("paipaR.xlsx",sep=""),1)

paipaR$MPIO_CCDGO= as.factor(paipaR$MPIO_CCDGO)
boyacafab <- boyaca_mapa %>% 
  left_join(paipaR)

names(paipaR)

boyacafabric <- boyacafab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                             st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                                2))
boyacafab <- cbind(boyacafab, st_coordinates(st_centroid(boyacafab$geometry)))#importante

ggplot(boyacafab)+ # tamaño 1000*800
  geom_sf(aes(fill=var))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = peso),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Valor agregado \n (miles de millones)", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(0.05,0.15,0.25,0.45,0.80,14))+ # nombres en colors()
  geom_text_repel(boyacafabric, mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(boyacafabric), datum = NA)+
  theme_void()

summary(paipaR$peso)

##########Municipios Visitados#####################
#####################################
ggplot(boyacafab)+ # tamaño 1000*800
  geom_sf(aes(fill=visitados2))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = visitados),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Número de personas", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 20), 
                        breaks = c(1,2,4,8,10,20))+ # nombres en colors()
  geom_text_repel(boyacafabric, mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(boyacafabric), datum = NA)+
  theme_void()

summary(paipaR$visitados)

##########Destino principal del viaje#####################
#####################################
ggplot(boyacafab)+ # tamaño 1000*800
  geom_sf(aes(fill=principal2))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = principal),
             colour = 'green', alpha = .5) +
  scale_fill_gradient("Número de personas", low="orange", high="orange4") +
  labs(size = 'Porcentaje')+ 
  scale_size_continuous(range = c(1, 20), 
                        breaks = c(1,2,4,8,10,20))+ # nombres en colors()
  geom_text_repel(boyacafabric, mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
  coord_sf(crs = st_crs(boyacafabric), datum = NA)+
  theme_void()

summary(paipaR$principal)


#############################################################
##########NO TIENE NADA QUE VER. MAPA DE RICAURTE. Pedro Pablo#####################
#############################################################
ggplot(boyacafab)+ # tamaño 1000*800
  geom_sf(aes(fill=ricaurte), show.legend = FALSE)+  xlab("") + ylab("") + 
  scale_fill_gradient("Número de personas", low="orange", high="orange4")+
  theme_void() 





#############################################################
##########Gasto en turismo en Paipa#####################
#############################################################
cat("\f")
rm(list = ls())

paipa <- read_dta("Documents/Investigacion/Juan Pablo Andrade/Estadisticas/Turistas Paipa.dta")
dim(paipa)
gastpai<- data.frame(paipa[,c(72:75)])
names(gastpai)
pai <- rename(gastpai, c(`Alojamiento`=X168, `Alimento y bebidas`=X170,`Actividades recreativas`=X172,`Artesanias`=X174))
pai2<-melt(pai)

v1<- filter(pai2, variable=="Alojamiento")
r<-summary(v1$value) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
v11 <- filter(v1, value<ati)

v2<- filter(pai2, variable=="Alimento y bebidas")
r<-summary(v2$value) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
v22 <- filter(v2, value<ati)

v3<- filter(pai2, variable=="Actividades recreativas")
r<-summary(v3$value) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
v33 <- filter(v3, value<ati)

v4<- filter(pai2, variable=="Artesanias")
r<-summary(v4$value) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
v44 <- filter(v4, value<ati)

paitot<-rbind(v11,v22,v33,v44)

ggplot(data = paitot, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('') + 
  ylab('$') + guides(fill=FALSE)   +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

summary(pai$Alojamiento) 
summary(v11$value) 
summary(v22$value) 
summary(v33$value) 
summary(v44$value) 

















#############################################################
##########Composición economica de Paipa#####################
#############################################################
cat("\f")
rm(list = ls())
economia <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim12.xlsx (1)/TerriData_Dim12.xlsx")

economiapaipa<- filter(economia, `Código Entidad`==15516 )
economiapaipa<- filter(economiapaipa, `Código Entidad`!=15000 )
economiapaipa<- data.frame(economiapaipa[,c(3:4,7:8,10)])

agricultura<- filter(economiapaipa, Indicador=="Agricultura, ganadería, caza, silvicultura y pesca")
minas<-filter(economiapaipa, Indicador=="Explotación de minas y canteras")
manufactura<-filter(economiapaipa, Indicador=="Industria manufacturera")
electricidad<-filter(economiapaipa, Indicador=="Suministro de electricidad, gas y agua")
construccion<-filter(economiapaipa, Indicador=="Construcción")
comercio<-filter(economiapaipa, Indicador=="Comercio, reparación, restaurantes y hoteles")
transporte<-filter(economiapaipa, Indicador=="Transporte, almacenamiento y comunicaciones")
finanzas<-filter(economiapaipa, Indicador=="Establecimientos financieros, seguros y otros servicios")
sociales<-filter(economiapaipa, Indicador=="Actividades de servicios sociales y personales")
percapita<-filter(economiapaipa, Indicador=="Valor agregado per cápita")
valor<-filter(economiapaipa, Indicador=="Valor agregado")

sectorespai<-rbind(agricultura,minas,manufactura,electricidad,construccion,comercio,transporte,finanzas,sociales)

write.csv(sectorespai, file = "sectorespai.csv")


cat("\f")
rm(list = ls())


paipaR3 <- read_excel(paste("paipaR.xlsx",sep=""),3)

acti2<-melt(paipaR3)

acti2<-melt(paipaR3, id.vars = c("Indicador","MPIO_CCDGO","Año","Entidad"))

acti2<-acti2%>%mutate(Porcentaje=round(value,2))
 
ggplot(acti2, aes(x = `Año`, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity") + geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})







##############################################################################
### Tendencias en Google Paipa##########
#######################Graficas de mapa y series de tiempo###################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################


#Codigo base 
# http://www.diegocalvo.es/obtener-datos-de-google-trends-con-r-usando-su-api/

#Importante para la edici?n
#https://www.rdocumentation.org/packages/rworldmap/versions/1.3-6/topics/mapCountryData

#Proposito del codigo
#Mapear el mundo y la frecuencia desde el 2005- turismo y gobernanza
#Mapear Colombia y la frecuencia desde el 2005- turismo y gobernanza
#No hacer wordclouds, mejor en el momento de redacci?n 




cat("\f")
rm(list = ls())

#CO = COLOMBIIA, US = USA


###################################################
# Tendencias en el mundo sobre gobernanza y turismo#
####################################################
#turismo
global_trends <- gtrends(c("Paipa"), gprop = "web", time = "2005-01-01 2019-12-31")
global_trends_by_country <- data.frame(country=global_trends$interest_by_country[,1], value=global_trends$interest_by_country[,2])
spdf <- joinCountryData2Map(global_trends_by_country, joinCode="NAME", nameJoinColumn="country")
mapCountryData(spdf, nameColumnToPlot="value",catMethod="fixedWidth", addLegend = FALSE, mapTitle = "") 

#Nota: extraer las palabras claves 

#http://rpubs.com/utjimmyx/429204
#Todo el mundo en turismo Tambien buscar con    youtube
es_trends1 <- gtrends(c("Paipa"), gprop = "web", time = "2005-01-01 2019-12-31")
plot(es_trends1)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra
View(es_trends1$interest_by_country[,1:2])


#Tendencia regional para Colombia  en turismo("tourism","governance") En ingles
es_trends2 <- gtrends(c("Paipa"), geo = c("CO"), gprop = "web", time = "2005-01-01 2019-12-01")
plot(es_trends2)+geom_line(size=0.5)+theme(legend.position = "bottom")#Aumentar tama?o de la letra
View(es_trends2$interest_by_region[,1:2])


uber_txt <- es_trends1$related_topics[,3]
uber_txt2<-es_trends1$related_queries[,3]
uber_txt3 <- es_trends2$related_topics[,3]
uber_txt4<-es_trends2$related_queries[,3]


uber_txt<-rbind(uber_txt,uber_txt2,uber_txt3,uber_txt4)

uber_corpus <- Corpus(VectorSource(uber_txt))
inspect(uber_corpus[1:10])
uber_corpus_clean <- tm_map(uber_corpus, tolower)
uber_corpus_clean <- tm_map(uber_corpus_clean, removeNumbers)
uber_corpus_clean <- tm_map(uber_corpus_clean, removePunctuation)
uber_corpus_clean <- tm_map(uber_corpus_clean, stripWhitespace)
stopwords(kind='en')#en ingles, es espa?ol
uber_corpus_clean <- tm_map(uber_corpus_clean, removeWords))
uber_tdm           <- TermDocumentMatrix(uber_corpus_clean, control = list(stopwords = TRUE))
uber_tdm           <- as.matrix(uber_tdm)

uber_dtm <- DocumentTermMatrix(uber_corpus_clean, control = list(minWordLength = 1, stopwords = TRUE))
inspect(uber_dtm)
uber_corpus_stem <- tm_map(uber_corpus_clean, stemDocument)
uber_corpus_stem <- tm_map(uber_corpus_stem, stemCompletion, dictionary = uber_corpus_clean)
inspect(uber_corpus_stem[1:5])
head(findFreqTerms(uber_dtm, lowfreq=10), 40)
findAssocs(uber_dtm, 'cabify', 0.40)
Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
uber_dict  <- Dictionary(findFreqTerms(uber_dtm, 5))
head(uber_dict)

#colores=brewer.pal(8,"Dark2")
wordcloud(uber_corpus_clean, random.order = TRUE, scale = c(8,0.5), min.freq = 1,
          max.words = 200,rot.per = 0, colors=brewer.pal(name = "Dark2", n = 8))
#









