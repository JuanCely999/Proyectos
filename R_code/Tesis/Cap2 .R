##############################################################################
###Capitulo 2 Tesis Pregrado en Economia################################
######################Autor: Juan Pablo Cely#################################
###############################25-07-2020####################################

library(foreign)
library(readxl)
library(ggplot2)  
library(readxl)
library(tidyr)

library(dplyr)
library(haven)
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
library(scales)
library(plotly)
library(RColorBrewer)
library(fmsb)

MesesCorrido <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/MesesCorridoCol2.dta")
summary(MesesCorrido$infanti)

anti<-summarise(MesesCorrido, media = mean(infanti), media1 = mean(banti), sd= sd(infanti), sd1= sd(banti), min=min(infanti), min1=min(banti), max=max(infanti), max1=max(banti))
atla<-summarise(MesesCorrido, media = mean(infatla), media1 = mean(batla), sd= sd(infatla), sd1= sd(batla), min=min(infatla), min1=min(batla), max=max(infatla), max1=max(batla))
bogo<-summarise(MesesCorrido, media = mean(infbogo), media1 = mean(bbogo), sd= sd(infbogo), sd1= sd(bbogo), min=min(infbogo), min1=min(bbogo), max=max(infbogo), max1=max(bbogo))
boli<-summarise(MesesCorrido, media = mean(infboli), media1 = mean(bboli), sd= sd(infboli), sd1= sd(bboli), min=min(infboli), min1=min(bboli), max=max(infboli), max1=max(bboli))
boya<-summarise(MesesCorrido, media = mean(infboya), media1 = mean(bboya), sd= sd(infboya), sd1= sd(bboya), min=min(infboya), min1=min(bboya), max=max(infboya), max1=max(bboya))
cald<-summarise(MesesCorrido, media = mean(infcald), media1 = mean(bcald), sd= sd(infcald), sd1= sd(bcald), min=min(infcald), min1=min(bcald), max=max(infcald), max1=max(bcald))
caqu<-summarise(MesesCorrido, media = mean(infcaqu), media1 = mean(bcaqu), sd= sd(infcaqu), sd1= sd(bcaqu), min=min(infcaqu), min1=min(bcaqu), max=max(infcaqu), max1=max(bcaqu))
cauc<-summarise(MesesCorrido, media = mean(infcauc), media1 = mean(bcauc), sd= sd(infcauc), sd1= sd(bcauc), min=min(infcauc), min1=min(bcauc), max=max(infcauc), max1=max(bcauc))
cesa<-summarise(MesesCorrido, media = mean(infcesa), media1 = mean(bcesa), sd= sd(infcesa), sd1= sd(bcesa), min=min(infcesa), min1=min(bcesa), max=max(infcesa), max1=max(bcesa))
cord<-summarise(MesesCorrido, media = mean(infcord), media1 = mean(bcord), sd= sd(infcord), sd1= sd(bcord), min=min(infcord), min1=min(bcord), max=max(infcord), max1=max(bcord))
choc<-summarise(MesesCorrido, media = mean(infchoc), media1 = mean(bchoc), sd= sd(infchoc), sd1= sd(bchoc), min=min(infchoc), min1=min(bchoc), max=max(infchoc), max1=max(bchoc))
cund<-summarise(MesesCorrido, media = mean(infbogo), media1 = mean(bcund), sd= sd(infbogo), sd1= sd(bcund), min=min(infbogo), min1=min(bcund), max=max(infbogo), max1=max(bcund))
huil<-summarise(MesesCorrido, media = mean(infhuil), media1 = mean(bhuil), sd= sd(infhuil), sd1= sd(bhuil), min=min(infhuil), min1=min(bhuil), max=max(infhuil), max1=max(bhuil))
lagua<-summarise(MesesCorrido, media = mean(inflagua), media1 = mean(blagua), sd= sd(inflagua), sd1= sd(blagua), min=min(inflagua), min1=min(blagua), max=max(inflagua), max1=max(blagua))
magd<-summarise(MesesCorrido, media = mean(infmagd), media1 = mean(bmagd), sd= sd(infmagd), sd1= sd(bmagd), min=min(infmagd), min1=min(bmagd), max=max(infmagd), max1=max(bmagd))
meta<-summarise(MesesCorrido, media = mean(infmeta), media1 = mean(bmeta), sd= sd(infmeta), sd1= sd(bmeta), min=min(infmeta), min1=min(bmeta), max=max(infmeta), max1=max(bmeta))
nari<-summarise(MesesCorrido, media = mean(infnari), media1 = mean(bnari), sd= sd(infnari), sd1= sd(bnari), min=min(infnari), min1=min(bnari), max=max(infnari), max1=max(bnari))
nort<-summarise(MesesCorrido, media = mean(infnort), media1 = mean(bnort), sd= sd(infnort), sd1= sd(bnort), min=min(infnort), min1=min(bnort), max=max(infnort), max1=max(bnort))
quin<-summarise(MesesCorrido, media = mean(infquin), media1 = mean(bquin), sd= sd(infquin), sd1= sd(bquin), min=min(infquin), min1=min(bquin), max=max(infquin), max1=max(bquin))
risa<-summarise(MesesCorrido, media = mean(infrisa), media1 = mean(brisa), sd= sd(infrisa), sd1= sd(brisa), min=min(infrisa), min1=min(brisa), max=max(infrisa), max1=max(brisa))
sant<-summarise(MesesCorrido, media = mean(infsant), media1 = mean(bsant), sd= sd(infsant), sd1= sd(bsant), min=min(infsant), min1=min(bsant), max=max(infsant), max1=max(bsant))
sucr<-summarise(MesesCorrido, media = mean(infsucr), media1 = mean(bsucr), sd= sd(infsucr), sd1= sd(bsucr), min=min(infsucr), min1=min(bsucr), max=max(infsucr), max1=max(bsucr))
toli<-summarise(MesesCorrido, media = mean(inftoli), media1 = mean(btoli), sd= sd(inftoli), sd1= sd(btoli), min=min(inftoli), min1=min(btoli), max=max(inftoli), max1=max(btoli))
vall<-summarise(MesesCorrido, media = mean(infvall), media1 = mean(bvall), sd= sd(infvall), sd1= sd(bvall), min=min(infvall), min1=min(bvall), max=max(infvall), max1=max(bvall))
colombia<-summarise(MesesCorrido, media = mean(inftotal), media1 = mean(bcolombia), sd= sd(inftotal), sd1= sd(bcolombia), min=min(inftotal), min1=min(bcolombia), max=max(inftotal), max1=max(bcolombia))

resumen<-rbind(anti,atla,bogo,boli,boya,cald,caqu,cauc,cesa,cord,choc,cund,
               huil,lagua,magd,meta,nari,nort,quin,risa,sant,sucr,toli,vall,
               colombia)

nombres<-c("anti","atla","bogo","boli","boya","cald","caqu","cauc","cesa","cord","choc","cund",
               "huil","lagua","magd","meta","nari","nort","quin","risa","sant","sucr","toli","vall",
               "colombia")
resument<-cbind(nombres,resumen)
write.csv(resument, file = "Descripcion.csv")



###############################################SE UTILIZA ESTE
###############################################

anti<-summarise(MesesCorrido, media = mean(infanti), media1 = mean(anticm), sd= sd(infanti), sd1= sd(anticm), min=min(infanti), min1=min(anticm), max=max(infanti), max1=max(anticm))
atla<-summarise(MesesCorrido, media = mean(infatla), media1 = mean(atlacm), sd= sd(infatla), sd1= sd(atlacm), min=min(infatla), min1=min(atlacm), max=max(infatla), max1=max(atlacm))
bogo<-summarise(MesesCorrido, media = mean(infbogo), media1 = mean(bogocm), sd= sd(infbogo), sd1= sd(bogocm), min=min(infbogo), min1=min(bogocm), max=max(infbogo), max1=max(bogocm))
boli<-summarise(MesesCorrido, media = mean(infboli), media1 = mean(bolicm), sd= sd(infboli), sd1= sd(bolicm), min=min(infboli), min1=min(bolicm), max=max(infboli), max1=max(bolicm))
boya<-summarise(MesesCorrido, media = mean(infboya), media1 = mean(boyacm), sd= sd(infboya), sd1= sd(boyacm), min=min(infboya), min1=min(boyacm), max=max(infboya), max1=max(boyacm))
cald<-summarise(MesesCorrido, media = mean(infcald), media1 = mean(caldcm), sd= sd(infcald), sd1= sd(caldcm), min=min(infcald), min1=min(caldcm), max=max(infcald), max1=max(caldcm))
caqu<-summarise(MesesCorrido, media = mean(infcaqu), media1 = mean(caqucm), sd= sd(infcaqu), sd1= sd(caqucm), min=min(infcaqu), min1=min(caqucm), max=max(infcaqu), max1=max(caqucm))
cauc<-summarise(MesesCorrido, media = mean(infcauc), media1 = mean(cauccm), sd= sd(infcauc), sd1= sd(cauccm), min=min(infcauc), min1=min(cauccm), max=max(infcauc), max1=max(cauccm))
cesa<-summarise(MesesCorrido, media = mean(infcesa), media1 = mean(cesacm), sd= sd(infcesa), sd1= sd(cesacm), min=min(infcesa), min1=min(cesacm), max=max(infcesa), max1=max(cesacm))
cord<-summarise(MesesCorrido, media = mean(infcord), media1 = mean(cordcm), sd= sd(infcord), sd1= sd(cordcm), min=min(infcord), min1=min(cordcm), max=max(infcord), max1=max(cordcm))
choc<-summarise(MesesCorrido, media = mean(infchoc), media1 = mean(choccm), sd= sd(infchoc), sd1= sd(choccm), min=min(infchoc), min1=min(choccm), max=max(infchoc), max1=max(choccm))
cund<-summarise(MesesCorrido, media = mean(infbogo), media1 = mean(cundcm), sd= sd(infbogo), sd1= sd(cundcm), min=min(infbogo), min1=min(cundcm), max=max(infbogo), max1=max(cundcm))
huil<-summarise(MesesCorrido, media = mean(infhuil), media1 = mean(huilcm), sd= sd(infhuil), sd1= sd(huilcm), min=min(infhuil), min1=min(huilcm), max=max(infhuil), max1=max(huilcm))
lagua<-summarise(MesesCorrido, media = mean(inflagua), media1 = mean(laguacm), sd= sd(inflagua), sd1= sd(laguacm), min=min(inflagua), min1=min(laguacm), max=max(inflagua), max1=max(laguacm))
magd<-summarise(MesesCorrido, media = mean(infmagd), media1 = mean(magdcm), sd= sd(infmagd), sd1= sd(magdcm), min=min(infmagd), min1=min(magdcm), max=max(infmagd), max1=max(magdcm))
meta<-summarise(MesesCorrido, media = mean(infmeta), media1 = mean(metacm), sd= sd(infmeta), sd1= sd(metacm), min=min(infmeta), min1=min(metacm), max=max(infmeta), max1=max(metacm))
nari<-summarise(MesesCorrido, media = mean(infnari), media1 = mean(naricm), sd= sd(infnari), sd1= sd(naricm), min=min(infnari), min1=min(naricm), max=max(infnari), max1=max(naricm))
nort<-summarise(MesesCorrido, media = mean(infnort), media1 = mean(nortcm), sd= sd(infnort), sd1= sd(nortcm), min=min(infnort), min1=min(nortcm), max=max(infnort), max1=max(nortcm))
quin<-summarise(MesesCorrido, media = mean(infquin), media1 = mean(quincm), sd= sd(infquin), sd1= sd(quincm), min=min(infquin), min1=min(quincm), max=max(infquin), max1=max(quincm))
risa<-summarise(MesesCorrido, media = mean(infrisa), media1 = mean(risacm), sd= sd(infrisa), sd1= sd(risacm), min=min(infrisa), min1=min(risacm), max=max(infrisa), max1=max(risacm))
sant<-summarise(MesesCorrido, media = mean(infsant), media1 = mean(santcm), sd= sd(infsant), sd1= sd(santcm), min=min(infsant), min1=min(santcm), max=max(infsant), max1=max(santcm))
sucr<-summarise(MesesCorrido, media = mean(infsucr), media1 = mean(sucrcm), sd= sd(infsucr), sd1= sd(sucrcm), min=min(infsucr), min1=min(sucrcm), max=max(infsucr), max1=max(sucrcm))
toli<-summarise(MesesCorrido, media = mean(inftoli), media1 = mean(tolicm), sd= sd(inftoli), sd1= sd(tolicm), min=min(inftoli), min1=min(tolicm), max=max(inftoli), max1=max(tolicm))
vall<-summarise(MesesCorrido, media = mean(infvall), media1 = mean(vallcm), sd= sd(infvall), sd1= sd(vallcm), min=min(infvall), min1=min(vallcm), max=max(infvall), max1=max(vallcm))
colombia<-summarise(MesesCorrido, media = mean(inftotal), media1 = mean(colombiacm), sd= sd(inftotal), sd1= sd(colombiacm), min=min(inftotal), min1=min(colombiacm), max=max(inftotal), max1=max(colombiacm))

resumen<-rbind(anti,atla,bogo,boli,boya,cald,caqu,cauc,cesa,cord,choc,cund,
               huil,lagua,magd,meta,nari,nort,quin,risa,sant,sucr,toli,vall,
               colombia)

nombres<-c("anti","atla","bogo","boli","boya","cald","caqu","cauc","cesa","cord","choc","cund",
           "huil","lagua","magd","meta","nari","nort","quin","risa","sant","sucr","toli","vall",
           "colombia")
resument2<-cbind(nombres,resumen)
write.csv(resument2, file = "Descripcioncm.csv")



cat("\f")
rm(list = ls())


###############################################Mapas de inflación y costos marginales
###############################################



######################Mapas costos laborales
colombia <-  st_read(dsn = "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/", layer = "depto")
setwd("~/Documents/Tesis/Referencias precios rigidos/Codigos R NPKC/")
coslab <- read_excel(paste("maptes.xlsx",sep=""),4)


colombia33 <- colombia %>% 
  left_join(coslab)


#colombia33[is.na(colombia33)] <- 0

colombia33ric <- colombia33 %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                             st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                                2))
colombia33 <- cbind(colombia33, st_coordinates(st_centroid(colombia33$geometry)))#importante

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2010), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2013), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2016), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2019), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()



####################Mapas inflación
cat("\f")
rm(list = ls())


infla <- read_excel(paste("maptes.xlsx",sep=""),5)

colombia <-  st_read(dsn = "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/", layer = "depto")



colombia33 <- colombia %>% 
  left_join(infla)


#colombia33[is.na(colombia33)] <- 0

colombia33ric <- colombia33 %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                           st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                              2))
colombia33 <- cbind(colombia33, st_coordinates(st_centroid(colombia33$geometry)))#importante

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2010), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2013), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2016), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=X2019), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()




#############################################3
########################Series de tiempo
cat("\f")
rm(list = ls())




#Crear fechas
meses <- c("01-01-10","02-01-10","03-01-10","04-01-10","05-01-10","06-01-10","07-01-10","08-01-10","09-01-10","10-01-10","11-01-10","12-01-10",
           "01-01-11","02-01-11","03-01-11","04-01-11","05-01-11","06-01-11","07-01-11","08-01-11","09-01-11","10-01-11","11-01-11","12-01-11",
           "01-01-12","02-01-12","03-01-12","04-01-12","05-01-12","06-01-12","07-01-12","08-01-12","09-01-12","10-01-12","11-01-12","12-01-12",
           "01-01-13","02-01-13","03-01-13","04-01-13","05-01-13","06-01-13","07-01-13","08-01-13","09-01-13","10-01-13","11-01-13","12-01-13",
           "01-01-14","02-01-14","03-01-14","04-01-14","05-01-14","06-01-14","07-01-14","08-01-14","09-01-14","10-01-14","11-01-14","12-01-14",
           "01-01-15","02-01-15","03-01-15","04-01-15","05-01-15","06-01-15","07-01-15","08-01-15","09-01-15","10-01-15","11-01-15","12-01-15",
           "01-01-16","02-01-16","03-01-16","04-01-16","05-01-16","06-01-16","07-01-16","08-01-16","09-01-16","10-01-16","11-01-16","12-01-16",
           "01-01-17","02-01-17","03-01-17","04-01-17","05-01-17","06-01-17","07-01-17","08-01-17","09-01-17","10-01-17","11-01-17","12-01-17",
           "01-01-18","02-01-18","03-01-18","04-01-18","05-01-18","06-01-18","07-01-18","08-01-18","09-01-18","10-01-18","11-01-18","12-01-18",
           "01-01-19","02-01-19","03-01-19","04-01-19","05-01-19","06-01-19","07-01-19","08-01-19","09-01-19","10-01-19","11-01-19","12-01-19")

meses <- data.frame(meses)


ciclos <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/MesesCorridoCol2.dta")
names(ciclos)

#################Antioquia
anti<- data.frame(ciclos[,c(27,52,51)])

anti$date <- as.Date(meses$meses, "%m-%d-%y")
anti$year <- year(anti$date)

anti2 <- rename(anti, c(`Inflación`=infanti,`Costos marginales (HP)`=banti, Antioquia=n))

anti3<- anti2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
anti4<-melt(anti3, id.vars = c("date","Variables","valor","year"))



#################Atlantico  
atla<- data.frame(ciclos[,c(28,54,51)])


atla$date <- as.Date(meses$meses, "%m-%d-%y")
atla$year <- year(atla$date)

atla2 <- rename(atla, c(`Inflación`=infatla,`Costos marginales (HP)`=batla, `Atlántico`=n))

atla3<- atla2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
atla4<-melt(atla3, id.vars = c("date","Variables","valor","year"))



#################Bogotá  
bogo<- data.frame(ciclos[,c(29,56,51)])


bogo$date <- as.Date(meses$meses, "%m-%d-%y")
bogo$year <- year(bogo$date)

bogo2 <- rename(bogo, c(`Inflación`=infbogo,`Costos marginales (HP)`=bbogo, `Bogotá`=n))

bogo3<- bogo2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
bogo4<-melt(bogo3, id.vars = c("date","Variables","valor","year"))




#################Bolívar  
boli<- data.frame(ciclos[,c(30,58,51)])


boli$date <- as.Date(meses$meses, "%m-%d-%y")
boli$year <- year(boli$date)

boli2 <- rename(boli, c(`Inflación`=infboli,`Costos marginales (HP)`=bboli, `Bolívar`=n))

boli3<- boli2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
boli4<-melt(boli3, id.vars = c("date","Variables","valor","year"))



#################Boyacá  
boya<- data.frame(ciclos[,c(31,60,51)])


boya$date <- as.Date(meses$meses, "%m-%d-%y")
boya$year <- year(boya$date)

boya2 <- rename(boya, c(`Inflación`=infboya,`Costos marginales (HP)`=bboya, `Boyacá`=n))

boya3<- boya2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
boya4<-melt(boya3, id.vars = c("date","Variables","valor","year"))


#################Caldas  
cald<- data.frame(ciclos[,c(32,62,51)])


cald$date <- as.Date(meses$meses, "%m-%d-%y")
cald$year <- year(cald$date)

cald2 <- rename(cald, c(`Inflación`=infcald,`Costos marginales (HP)`=bcald, Caldas=n))

cald3<- cald2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
cald4<-melt(cald3, id.vars = c("date","Variables","valor","year"))





#################Caquetá  
caqu<- data.frame(ciclos[,c(33,64,51)])


caqu$date <- as.Date(meses$meses, "%m-%d-%y")
caqu$year <- year(caqu$date)

caqu2 <- rename(caqu, c(`Inflación`=infcaqu,`Costos marginales (HP)`=bcaqu, `Caquetá`=n))

caqu3<- caqu2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
caqu4<-melt(caqu3, id.vars = c("date","Variables","valor","year"))


#################Cauca  
cauc<- data.frame(ciclos[,c(34,66,51)])


cauc$date <- as.Date(meses$meses, "%m-%d-%y")
cauc$year <- year(cauc$date)

cauc2 <- rename(cauc, c(`Inflación`=infcauc,`Costos marginales (HP)`=bcauc, Cauca=n))

cauc3<- cauc2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
cauc4<-melt(cauc3, id.vars = c("date","Variables","valor","year"))



#################Cesar  
cesa<- data.frame(ciclos[,c(35,68,51)])


cesa$date <- as.Date(meses$meses, "%m-%d-%y")
cesa$year <- year(cesa$date)

cesa2 <- rename(cesa, c(`Inflación`=infcesa,`Costos marginales (HP)`=bcesa, Cesar=n))

cesa3<- cesa2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
cesa4<-melt(cesa3, id.vars = c("date","Variables","valor","year"))



#################Córdoba  
cord<- data.frame(ciclos[,c(36,70,51)])


cord$date <- as.Date(meses$meses, "%m-%d-%y")
cord$year <- year(cord$date)

cord2 <- rename(cord, c(`Inflación`=infcord,`Costos marginales (HP)`=bcord, `Córdoba`=n))

cord3<- cord2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
cord4<-melt(cord3, id.vars = c("date","Variables","valor","year"))


#################Chocó  
choc<- data.frame(ciclos[,c(37,74,51)])


choc$date <- as.Date(meses$meses, "%m-%d-%y")
choc$year <- year(choc$date)

choc2 <- rename(choc, c(`Inflación`=infchoc,`Costos marginales (HP)`=bchoc, `Chocó`=n))

choc3<- choc2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
choc4<-melt(choc3, id.vars = c("date","Variables","valor","year"))


#################Cundinamarca  
cund<- data.frame(ciclos[,c(29,72,51)])


cund$date <- as.Date(meses$meses, "%m-%d-%y")
cund$year <- year(cund$date)

cund2 <- rename(cund, c(`Inflación`=infbogo,`Costos marginales (HP)`=bcund, Cundinamarca=n))

cund3<- cund2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
cund4<-melt(cund3, id.vars = c("date","Variables","valor","year"))


#################Huila  
huil<- data.frame(ciclos[,c(38,76,51)])


huil$date <- as.Date(meses$meses, "%m-%d-%y")
huil$year <- year(huil$date)

huil2 <- rename(huil, c(`Inflación`=infhuil,`Costos marginales (HP)`=bhuil, Huila=n))

huil3<- huil2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
huil4<-melt(huil3, id.vars = c("date","Variables","valor","year"))

#################La Guajira  
lagua<- data.frame(ciclos[,c(39,78,51)])


lagua$date <- as.Date(meses$meses, "%m-%d-%y")
lagua$year <- year(lagua$date)

lagua2 <- rename(lagua, c(`Inflación`=inflagua,`Costos marginales (HP)`=blagua, `La Guajira`=n))

lagua3<- lagua2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
lagua4<-melt(lagua3, id.vars = c("date","Variables","valor","year"))


#################Magdalena  
magd<- data.frame(ciclos[,c(40,80,51)])


magd$date <- as.Date(meses$meses, "%m-%d-%y")
magd$year <- year(magd$date)

magd2 <- rename(magd, c(`Inflación`=infmagd,`Costos marginales (HP)`=bmagd, Magdalena=n))

magd3<- magd2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
magd4<-melt(magd3, id.vars = c("date","Variables","valor","year"))


#################Meta  
meta<- data.frame(ciclos[,c(41,82,51)])


meta$date <- as.Date(meses$meses, "%m-%d-%y")
meta$year <- year(meta$date)

meta2 <- rename(meta, c(`Inflación`=infmeta,`Costos marginales (HP)`=bmeta, Meta=n))

meta3<- meta2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
meta4<-melt(meta3, id.vars = c("date","Variables","valor","year"))


#################Nariño  
nari<- data.frame(ciclos[,c(42,84,51)])


nari$date <- as.Date(meses$meses, "%m-%d-%y")
nari$year <- year(nari$date)

nari2 <- rename(nari, c(`Inflación`=infnari,`Costos marginales (HP)`=bnari, `Nariño`=n))

nari3<- nari2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
nari4<-melt(nari3, id.vars = c("date","Variables","valor","year"))


#################N. Santander  
nort<- data.frame(ciclos[,c(43,86,51)])


nort$date <- as.Date(meses$meses, "%m-%d-%y")
nort$year <- year(nort$date)

nort2 <- rename(nort, c(`Inflación`=infnort,`Costos marginales (HP)`=bnort, `N. Santander`=n))

nort3<- nort2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
nort4<-melt(nort3, id.vars = c("date","Variables","valor","year"))


#################Quindío  
quin<- data.frame(ciclos[,c(44,88,51)])


quin$date <- as.Date(meses$meses, "%m-%d-%y")
quin$year <- year(quin$date)

quin2 <- rename(quin, c(`Inflación`=infquin,`Costos marginales (HP)`=bquin, `Quindío`=n))

quin3<- quin2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
quin4<-melt(quin3, id.vars = c("date","Variables","valor","year"))


#################Risaralda  
risa<- data.frame(ciclos[,c(45,90,51)])


risa$date <- as.Date(meses$meses, "%m-%d-%y")
risa$year <- year(risa$date)

risa2 <- rename(risa, c(`Inflación`=infrisa,`Costos marginales (HP)`=brisa, Risaralda=n))

risa3<- risa2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
risa4<-melt(risa3, id.vars = c("date","Variables","valor","year"))


################Santander  
sant<- data.frame(ciclos[,c(46,92,51)])


sant$date <- as.Date(meses$meses, "%m-%d-%y")
sant$year <- year(sant$date)

sant2 <- rename(sant, c(`Inflación`=infsant,`Costos marginales (HP)`=bsant, Santander=n))

sant3<- sant2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
sant4<-melt(sant3, id.vars = c("date","Variables","valor","year"))


################Sucre  
sucr<- data.frame(ciclos[,c(47,94,51)])


sucr$date <- as.Date(meses$meses, "%m-%d-%y")
sucr$year <- year(sucr$date)

sucr2 <- rename(sucr, c(`Inflación`=infsucr,`Costos marginales (HP)`=bsucr, Sucre=n))

sucr3<- sucr2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
sucr4<-melt(sucr3, id.vars = c("date","Variables","valor","year"))


################Tolima  
toli<- data.frame(ciclos[,c(48,96,51)])


toli$date <- as.Date(meses$meses, "%m-%d-%y")
toli$year <- year(toli$date)

toli2 <- rename(toli, c(`Inflación`=inftoli,`Costos marginales (HP)`=btoli, Tolima=n))

toli3<- toli2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
toli4<-melt(toli3, id.vars = c("date","Variables","valor","year"))


################V. Cauca  
vall<- data.frame(ciclos[,c(49,98,51)])


vall$date <- as.Date(meses$meses, "%m-%d-%y")
vall$year <- year(vall$date)

vall2 <- rename(vall, c(`Inflación`=infvall,`Costos marginales (HP)`=bvall, `V. Cauca`=n))

vall3<- vall2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
vall4<-melt(vall3, id.vars = c("date","Variables","valor","year"))



################Todas las regiones 
colombia<- data.frame(ciclos[,c(50,100,51)])


colombia$date <- as.Date(meses$meses, "%m-%d-%y")
colombia$year <- year(colombia$date)

colombia2 <- rename(colombia, c(`Inflación`=inftotal,`Costos marginales (HP)`=bcolombia, `Todas las regiones`=n))

colombia3<- colombia2 %>% gather("Inflación","Costos marginales (HP)",key="Variables",value="valor",-year) 
colombia4<-melt(colombia3, id.vars = c("date","Variables","valor","year"))








####################Visualizaciones finales

ciclo5<-rbind(anti4,atla4,bogo4,boli4,boya4,cald4,caqu4,cauc4,cesa4,cord4,cund4,choc4,huil4,lagua4,magd4,
              meta4,nari4,nort4,quin4,risa4,sant4,sucr4,toli4,vall4,colombia4)

ggplot(data = ciclo5, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1) + xlab("") + ylab("") +
  scale_color_manual(values = c("#FC4E07","#E7B800")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(.~variable)#https://www.w3schools.com/sql/func_mysql_date_format.asp   para cambiar formatos "%Y"

#linetype= Variables MODIFICACIÓN PARA LE REVISTA




###########################Correlaciones cruzadas##############

cat("\f")
rm(list = ls())
MesesCorrido <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/MesesCorridoCol2.dta")


#MesesCorrido <- 
#  ts(MesesCorrido,   #La series es mensual, 12 puntos de datos.   
#     start=2010,frequency = 12)

#Antioquia
anti2 <- ccf(MesesCorrido[,"infanti"],MesesCorrido[,"banti"], lag = 20, plot = FALSE)
anti3 <- with(anti2, data.frame(lag, acf))
anti4 <- rep("Antioquia" , 41)
anticm<-cbind(anti3,anti4)
names(anticm)= c("lag", "acf", "nombre")

#Atlántico
atla2 <- ccf(MesesCorrido[,"infatla"],MesesCorrido[,"batla"], lag = 20, plot = FALSE)
atla3 <- with(atla2, data.frame(lag, acf))
atla4 <- rep("Atlántico" , 41)
atlacm<-cbind(atla3,atla4)
names(atlacm)= c("lag", "acf", "nombre")

#Bogotá
bogo2 <- ccf(MesesCorrido[,"infbogo"],MesesCorrido[,"bbogo"], lag = 20, plot = FALSE)
bogo3 <- with(bogo2, data.frame(lag, acf))
bogo4 <- rep("Bogotá" , 41)
bogocm<-cbind(bogo3,bogo4)
names(bogocm)= c("lag", "acf", "nombre")

#Bolívar
boli2 <- ccf(MesesCorrido[,"infboli"],MesesCorrido[,"bboli"], lag = 20, plot = FALSE)
boli3 <- with(boli2, data.frame(lag, acf))
boli4 <- rep("Bolívar" , 41)
bolicm<-cbind(boli3,boli4)
names(bolicm)= c("lag", "acf", "nombre")

#Boyacá
boya2 <- ccf(MesesCorrido[,"infboya"],MesesCorrido[,"bboya"], lag = 20, plot = FALSE)
boya3 <- with(boya2, data.frame(lag, acf))
boya4 <- rep("Boyacá" , 41)
boyacm<-cbind(boya3,boya4)
names(boyacm)= c("lag", "acf", "nombre")

#Caldas
cald2 <- ccf(MesesCorrido[,"infcald"],MesesCorrido[,"bcald"], lag = 20, plot = FALSE)
cald3 <- with(cald2, data.frame(lag, acf))
cald4 <- rep("Caldas" , 41)
caldcm<-cbind(cald3,cald4)
names(caldcm)= c("lag", "acf", "nombre")

#Caquetá
caqu2 <- ccf(MesesCorrido[,"infcaqu"],MesesCorrido[,"bcaqu"], lag = 20, plot = FALSE)
caqu3 <- with(caqu2, data.frame(lag, acf))
caqu4 <- rep("Caquetá" , 41)
caqucm<-cbind(caqu3,caqu4)
names(caqucm)= c("lag", "acf", "nombre")

#Cauca
cauc2 <- ccf(MesesCorrido[,"infcauc"],MesesCorrido[,"bcauc"], lag = 20, plot = FALSE)
cauc3 <- with(cauc2, data.frame(lag, acf))
cauc4 <- rep("Cauca" , 41)
cauccm<-cbind(cauc3,cauc4)
names(cauccm)= c("lag", "acf", "nombre")

#Cesar
cesa2 <- ccf(MesesCorrido[,"infcesa"],MesesCorrido[,"bcesa"], lag = 20, plot = FALSE)
cesa3 <- with(cesa2, data.frame(lag, acf))
cesa4 <- rep("Cesar" , 41)
cesacm<-cbind(cesa3,cesa4)
names(cesacm)= c("lag", "acf", "nombre")

#Córdoba
cord2 <- ccf(MesesCorrido[,"infcord"],MesesCorrido[,"bcord"], lag = 20, plot = FALSE)
cord3 <- with(cord2, data.frame(lag, acf))
cord4 <- rep("Córdoba" , 41)
cordcm<-cbind(cord3,cord4)
names(cordcm)= c("lag", "acf", "nombre")

#Cundinamarca
cund2 <- ccf(MesesCorrido[,"infbogo"],MesesCorrido[,"bcund"], lag = 20, plot = FALSE)
cund3 <- with(cund2, data.frame(lag, acf))
cund4 <- rep("Cundinamarca" , 41)
cundcm<-cbind(cund3,cund4)
names(cundcm)= c("lag", "acf", "nombre")

#Chocó totempresas[is.na(totempresas)] <- 0
chocmej<-MesesCorrido$infchoc
chocmej2<-MesesCorrido$bchoc
choctot<-cbind(chocmej,chocmej2)
choctot[is.na(choctot)] <- 0

choc2 <- ccf(choctot[,"chocmej"],choctot[,"chocmej2"], lag = 20, plot = FALSE)
choc3 <- with(choc2, data.frame(lag, acf))
choc4 <- rep("Chocó" , 41)
choccm<-cbind(choc3,choc4)
names(choccm)= c("lag", "acf", "nombre")

#Huila
huil2 <- ccf(MesesCorrido[,"infhuil"],MesesCorrido[,"bhuil"], lag = 20, plot = FALSE)
huil3 <- with(huil2, data.frame(lag, acf))
huil4 <- rep("Huila" , 41)
huilcm<-cbind(huil3,huil4)
names(huilcm)= c("lag", "acf", "nombre")

#La Guajira
lagua2 <- ccf(MesesCorrido[,"inflagua"],MesesCorrido[,"blagua"], lag = 20, plot = FALSE)
lagua3 <- with(lagua2, data.frame(lag, acf))
lagua4 <- rep("La Guajira" , 41)
laguacm<-cbind(lagua3,lagua4)
names(laguacm)= c("lag", "acf", "nombre")

#Magdalena
magd2 <- ccf(MesesCorrido[,"infmagd"],MesesCorrido[,"bmagd"], lag = 20, plot = FALSE)
magd3 <- with(magd2, data.frame(lag, acf))
magd4 <- rep("Magdalena" , 41)
magdcm<-cbind(magd3,magd4)
names(magdcm)= c("lag", "acf", "nombre")

#Meta
meta2 <- ccf(MesesCorrido[,"infmeta"],MesesCorrido[,"bmeta"], lag = 20, plot = FALSE)
meta3 <- with(meta2, data.frame(lag, acf))
meta4 <- rep("Meta" , 41)
metacm<-cbind(meta3,meta4)
names(metacm)= c("lag", "acf", "nombre")

#Nariño
nari2 <- ccf(MesesCorrido[,"infnari"],MesesCorrido[,"bnari"], lag = 20, plot = FALSE)
nari3 <- with(nari2, data.frame(lag, acf))
nari4 <- rep("Nariño" , 41)
naricm<-cbind(nari3,nari4)
names(naricm)= c("lag", "acf", "nombre")

#N. Santander
nort2 <- ccf(MesesCorrido[,"infnort"],MesesCorrido[,"bnort"], lag = 20, plot = FALSE)
nort3 <- with(nort2, data.frame(lag, acf))
nort4 <- rep("N. Santander" , 41)
nortcm<-cbind(nort3,nort4)
names(nortcm)= c("lag", "acf", "nombre")


#Quindío
quin2 <- ccf(MesesCorrido[,"infquin"],MesesCorrido[,"bquin"], lag = 20, plot = FALSE)
quin3 <- with(quin2, data.frame(lag, acf))
quin4 <- rep("Quindío" , 41)
quincm<-cbind(quin3,quin4)
names(quincm)= c("lag", "acf", "nombre")

#Risaralda
risa2 <- ccf(MesesCorrido[,"infrisa"],MesesCorrido[,"brisa"], lag = 20, plot = FALSE)
risa3 <- with(risa2, data.frame(lag, acf))
risa4 <- rep("Risaralda" , 41)
risacm<-cbind(risa3,risa4)
names(risacm)= c("lag", "acf", "nombre")

#Santander
sant2 <- ccf(MesesCorrido[,"infsant"],MesesCorrido[,"bsant"], lag = 20, plot = FALSE)
sant3 <- with(sant2, data.frame(lag, acf))
sant4 <- rep("Santander" , 41)
santcm<-cbind(sant3,sant4)
names(santcm)= c("lag", "acf", "nombre")

#Sucre
sucr2 <- ccf(MesesCorrido[,"infsucr"],MesesCorrido[,"bsucr"], lag = 20, plot = FALSE)
sucr3 <- with(sucr2, data.frame(lag, acf))
sucr4 <- rep("Sucre" , 41)
sucrcm<-cbind(sucr3,sucr4)
names(sucrcm)= c("lag", "acf", "nombre")

#Tolima
toli2 <- ccf(MesesCorrido[,"inftoli"],MesesCorrido[,"btoli"], lag = 20, plot = FALSE)
toli3 <- with(toli2, data.frame(lag, acf))
toli4 <- rep("Tolima" , 41)
tolicm<-cbind(toli3,toli4)
names(tolicm)= c("lag", "acf", "nombre")


#V. Cauca
vall2 <- ccf(MesesCorrido[,"infvall"],MesesCorrido[,"bvall"], lag = 20, plot = FALSE)
vall3 <- with(vall2, data.frame(lag, acf))
vall4 <- rep("V. Cauca" , 41)
vallcm<-cbind(vall3,vall4)
names(vallcm)= c("lag", "acf", "nombre")

#Todas las regiones
colombia2 <- ccf(MesesCorrido[,"inftotal"],MesesCorrido[,"bcolombia"], lag = 20, plot = FALSE)
colombia3 <- with(colombia2, data.frame(lag, acf))
colombia4 <- rep("Todas las regiones" , 41)
colombiacm<-cbind(colombia3,colombia4)
names(colombiacm)= c("lag", "acf", "nombre")


correla<-rbind(anticm,atlacm,bogocm,bolicm,boyacm,caldcm,caqucm,cauccm,cesacm,cordcm,cundcm,choccm,huilcm,laguacm,magdcm,
              metacm,naricm,nortcm,quincm,risacm,santcm,sucrcm,tolicm,vallcm,colombiacm)



ggplot(data=correla, aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+geom_hline( yintercept=0.5)+geom_hline( yintercept=-0.5)+
  scale_x_continuous(breaks=seq(-20,20,5))+ xlab("")+ ylab("")+
  facet_wrap(.~nombre)+ theme_bw() 




###############################################Mapas de los residuos
############################################################
cat("\f")
rm(list = ls())

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
library(RColorBrewer)
library(fmsb)
library(reshape2)

#Crear fechas
meses <- c("01-01-10","02-01-10","03-01-10","04-01-10","05-01-10","06-01-10","07-01-10","08-01-10","09-01-10","10-01-10","11-01-10","12-01-10",
           "01-01-11","02-01-11","03-01-11","04-01-11","05-01-11","06-01-11","07-01-11","08-01-11","09-01-11","10-01-11","11-01-11","12-01-11",
           "01-01-12","02-01-12","03-01-12","04-01-12","05-01-12","06-01-12","07-01-12","08-01-12","09-01-12","10-01-12","11-01-12","12-01-12",
           "01-01-13","02-01-13","03-01-13","04-01-13","05-01-13","06-01-13","07-01-13","08-01-13","09-01-13","10-01-13","11-01-13","12-01-13",
           "01-01-14","02-01-14","03-01-14","04-01-14","05-01-14","06-01-14","07-01-14","08-01-14","09-01-14","10-01-14","11-01-14","12-01-14",
           "01-01-15","02-01-15","03-01-15","04-01-15","05-01-15","06-01-15","07-01-15","08-01-15","09-01-15","10-01-15","11-01-15","12-01-15",
           "01-01-16","02-01-16","03-01-16","04-01-16","05-01-16","06-01-16","07-01-16","08-01-16","09-01-16","10-01-16","11-01-16","12-01-16",
           "01-01-17","02-01-17","03-01-17","04-01-17","05-01-17","06-01-17","07-01-17","08-01-17","09-01-17","10-01-17","11-01-17","12-01-17",
           "01-01-18","02-01-18","03-01-18","04-01-18","05-01-18","06-01-18","07-01-18","08-01-18","09-01-18","10-01-18","11-01-18","12-01-18",
           "01-01-19","02-01-19","03-01-19","04-01-19","05-01-19","06-01-19","07-01-19","08-01-19","09-01-19","10-01-19","11-01-19","12-01-19")

meses <- data.frame(meses)


ciclos <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/Residuos.dta")
names(ciclos)


#################Antioquia
anti<- data.frame(ciclos[,c(102,51)])

anti$date <- as.Date(meses$meses, "%m-%d-%y")
anti$year <- year(anti$date)

anti2 <- rename(anti, c(`Inflación`=ant,Antioquia=n))


anti3<- anti2 %>% gather("Inflación",key="Variables",value="valor",-year) 
anti4<-melt(anti3, id.vars = c("date","Variables","valor","year"))



#################Atlantico  
atla<- data.frame(ciclos[,c(103,51)])


atla$date <- as.Date(meses$meses, "%m-%d-%y")
atla$year <- year(atla$date)

atla2 <- rename(atla, c(`Inflación`=atla,`Atlántico`=n))

atla3<- atla2 %>% gather("Inflación",key="Variables",value="valor",-year) 
atla4<-melt(atla3, id.vars = c("date","Variables","valor","year"))



#################Bogotá  
bogo<- data.frame(ciclos[,c(104,51)])


bogo$date <- as.Date(meses$meses, "%m-%d-%y")
bogo$year <- year(bogo$date)

bogo2 <- rename(bogo, c(`Inflación`=bogo,`Bogotá`=n))

bogo3<- bogo2 %>% gather("Inflación",key="Variables",value="valor",-year) 
bogo4<-melt(bogo3, id.vars = c("date","Variables","valor","year"))




#################Bolívar  
boli<- data.frame(ciclos[,c(105,51)])


boli$date <- as.Date(meses$meses, "%m-%d-%y")
boli$year <- year(boli$date)

boli2 <- rename(boli, c(`Inflación`=boli,`Bolívar`=n))

boli3<- boli2 %>% gather("Inflación",key="Variables",value="valor",-year) 
boli4<-melt(boli3, id.vars = c("date","Variables","valor","year"))



#################Boyacá  
boya<- data.frame(ciclos[,c(106,51)])


boya$date <- as.Date(meses$meses, "%m-%d-%y")
boya$year <- year(boya$date)

boya2 <- rename(boya, c(`Inflación`=boya,`Boyacá`=n))

boya3<- boya2 %>% gather("Inflación",key="Variables",value="valor",-year) 
boya4<-melt(boya3, id.vars = c("date","Variables","valor","year"))


#################Caldas  
cald<- data.frame(ciclos[,c(107,51)])


cald$date <- as.Date(meses$meses, "%m-%d-%y")
cald$year <- year(cald$date)

cald2 <- rename(cald, c(`Inflación`=cald,Caldas=n))

cald3<- cald2 %>% gather("Inflación",key="Variables",value="valor",-year) 
cald4<-melt(cald3, id.vars = c("date","Variables","valor","year"))





#################Caquetá  
caqu<- data.frame(ciclos[,c(108,51)])


caqu$date <- as.Date(meses$meses, "%m-%d-%y")
caqu$year <- year(caqu$date)

caqu2 <- rename(caqu, c(`Inflación`=caqu,`Caquetá`=n))

caqu3<- caqu2 %>% gather("Inflación",key="Variables",value="valor",-year) 
caqu4<-melt(caqu3, id.vars = c("date","Variables","valor","year"))


#################Cauca  
cauc<- data.frame(ciclos[,c(109,51)])


cauc$date <- as.Date(meses$meses, "%m-%d-%y")
cauc$year <- year(cauc$date)

cauc2 <- rename(cauc, c(`Inflación`=cauc,Cauca=n))

cauc3<- cauc2 %>% gather("Inflación",key="Variables",value="valor",-year) 
cauc4<-melt(cauc3, id.vars = c("date","Variables","valor","year"))



#################Cesar  
cesa<- data.frame(ciclos[,c(110,51)])


cesa$date <- as.Date(meses$meses, "%m-%d-%y")
cesa$year <- year(cesa$date)

cesa2 <- rename(cesa, c(`Inflación`=cesa,Cesar=n))

cesa3<- cesa2 %>% gather("Inflación",key="Variables",value="valor",-year) 
cesa4<-melt(cesa3, id.vars = c("date","Variables","valor","year"))



#################Córdoba  
cord<- data.frame(ciclos[,c(111,51)])


cord$date <- as.Date(meses$meses, "%m-%d-%y")
cord$year <- year(cord$date)

cord2 <- rename(cord, c(`Inflación`=cord,`Córdoba`=n))

cord3<- cord2 %>% gather("Inflación",key="Variables",value="valor",-year) 
cord4<-melt(cord3, id.vars = c("date","Variables","valor","year"))


#################Chocó  
choc<- data.frame(ciclos[,c(113,51)])


choc$date <- as.Date(meses$meses, "%m-%d-%y")
choc$year <- year(choc$date)

choc2 <- rename(choc, c(`Inflación`=choc,`Chocó`=n))

choc3<- choc2 %>% gather("Inflación",key="Variables",value="valor",-year) 
choc4<-melt(choc3, id.vars = c("date","Variables","valor","year"))


#################Cundinamarca  
cund<- data.frame(ciclos[,c(112,51)])


cund$date <- as.Date(meses$meses, "%m-%d-%y")
cund$year <- year(cund$date)

cund2 <- rename(cund, c(`Inflación`=cund,Cundinamarca=n))

cund3<- cund2 %>% gather("Inflación",key="Variables",value="valor",-year) 
cund4<-melt(cund3, id.vars = c("date","Variables","valor","year"))


#################Huila  
huil<- data.frame(ciclos[,c(114,51)])


huil$date <- as.Date(meses$meses, "%m-%d-%y")
huil$year <- year(huil$date)

huil2 <- rename(huil, c(`Inflación`=huil,Huila=n))

huil3<- huil2 %>% gather("Inflación",key="Variables",value="valor",-year) 
huil4<-melt(huil3, id.vars = c("date","Variables","valor","year"))

#################La Guajira  
lagua<- data.frame(ciclos[,c(115,51)])


lagua$date <- as.Date(meses$meses, "%m-%d-%y")
lagua$year <- year(lagua$date)

lagua2 <- rename(lagua, c(`Inflación`=lagua,`La Guajira`=n))

lagua3<- lagua2 %>% gather("Inflación",key="Variables",value="valor",-year) 
lagua4<-melt(lagua3, id.vars = c("date","Variables","valor","year"))


#################Magdalena  
magd<- data.frame(ciclos[,c(116,51)])


magd$date <- as.Date(meses$meses, "%m-%d-%y")
magd$year <- year(magd$date)

magd2 <- rename(magd, c(`Inflación`=magd,Magdalena=n))

magd3<- magd2 %>% gather("Inflación",key="Variables",value="valor",-year) 
magd4<-melt(magd3, id.vars = c("date","Variables","valor","year"))


#################Meta  
meta<- data.frame(ciclos[,c(117,51)])


meta$date <- as.Date(meses$meses, "%m-%d-%y")
meta$year <- year(meta$date)

meta2 <- rename(meta, c(`Inflación`=meta,Meta=n))

meta3<- meta2 %>% gather("Inflación",key="Variables",value="valor",-year) 
meta4<-melt(meta3, id.vars = c("date","Variables","valor","year"))


#################Nariño  
nari<- data.frame(ciclos[,c(118,51)])


nari$date <- as.Date(meses$meses, "%m-%d-%y")
nari$year <- year(nari$date)

nari2 <- rename(nari, c(`Inflación`=nari,`Nariño`=n))

nari3<- nari2 %>% gather("Inflación",key="Variables",value="valor",-year) 
nari4<-melt(nari3, id.vars = c("date","Variables","valor","year"))


#################N. Santander  
nort<- data.frame(ciclos[,c(119,51)])


nort$date <- as.Date(meses$meses, "%m-%d-%y")
nort$year <- year(nort$date)

nort2 <- rename(nort, c(`Inflación`=nort,`N. Santander`=n))

nort3<- nort2 %>% gather("Inflación",key="Variables",value="valor",-year) 
nort4<-melt(nort3, id.vars = c("date","Variables","valor","year"))


#################Quindío  
quin<- data.frame(ciclos[,c(120,51)])


quin$date <- as.Date(meses$meses, "%m-%d-%y")
quin$year <- year(quin$date)

quin2 <- rename(quin, c(`Inflación`=quin,`Quindío`=n))

quin3<- quin2 %>% gather("Inflación",key="Variables",value="valor",-year) 
quin4<-melt(quin3, id.vars = c("date","Variables","valor","year"))


#################Risaralda  
risa<- data.frame(ciclos[,c(121,51)])


risa$date <- as.Date(meses$meses, "%m-%d-%y")
risa$year <- year(risa$date)

risa2 <- rename(risa, c(`Inflación`=risa,Risaralda=n))

risa3<- risa2 %>% gather("Inflación",key="Variables",value="valor",-year) 
risa4<-melt(risa3, id.vars = c("date","Variables","valor","year"))


################Santander  
sant<- data.frame(ciclos[,c(122,51)])


sant$date <- as.Date(meses$meses, "%m-%d-%y")
sant$year <- year(sant$date)

sant2 <- rename(sant, c(`Inflación`=sant,Santander=n))

sant3<- sant2 %>% gather("Inflación",key="Variables",value="valor",-year) 
sant4<-melt(sant3, id.vars = c("date","Variables","valor","year"))


################Sucre  
sucr<- data.frame(ciclos[,c(123,51)])


sucr$date <- as.Date(meses$meses, "%m-%d-%y")
sucr$year <- year(sucr$date)

sucr2 <- rename(sucr, c(`Inflación`=sucr,Sucre=n))

sucr3<- sucr2 %>% gather("Inflación",key="Variables",value="valor",-year) 
sucr4<-melt(sucr3, id.vars = c("date","Variables","valor","year"))


################Tolima  
toli<- data.frame(ciclos[,c(124,51)])


toli$date <- as.Date(meses$meses, "%m-%d-%y")
toli$year <- year(toli$date)

toli2 <- rename(toli, c(`Inflación`=toli,Tolima=n))

toli3<- toli2 %>% gather("Inflación",key="Variables",value="valor",-year) 
toli4<-melt(toli3, id.vars = c("date","Variables","valor","year"))


################V. Cauca  
vall<- data.frame(ciclos[,c(125,51)])


vall$date <- as.Date(meses$meses, "%m-%d-%y")
vall$year <- year(vall$date)

vall2 <- rename(vall, c(`Inflación`=vall,`V. Cauca`=n))

vall3<- vall2 %>% gather("Inflación",key="Variables",value="valor",-year) 
vall4<-melt(vall3, id.vars = c("date","Variables","valor","year"))



################Todas las regiones 
colombia<- data.frame(ciclos[,c(126,51)])


colombia$date <- as.Date(meses$meses, "%m-%d-%y")
colombia$year <- year(colombia$date)

colombia2 <- rename(colombia, c(`Inflación`=colo,`Todas las regiones`=n))

colombia3<- colombia2 %>% gather("Inflación",key="Variables",value="valor",-year) 
colombia4<-melt(colombia3, id.vars = c("date","Variables","valor","year"))



####################Visualizaciones finales

ciclo5<-rbind(anti4,atla4,bogo4,boli4,boya4,cald4,caqu4,cauc4,cesa4,cord4,cund4,choc4,huil4,lagua4,magd4,
              meta4,nari4,nort4,quin4,risa4,sant4,sucr4,toli4,vall4,colombia4)

ggplot(data = ciclo5, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1, show.legend = FALSE) + xlab("") + ylab("") +
  scale_color_manual(values = c("#E7B800")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(.~variable)#https://www.w3schools.com/sql/func_mysql_date_format.asp   para cambiar formatos "%Y"





###############################################Mapas de resultados
###############################################
cat("\f")
rm(list = ls())
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



######################Mapas costos laborales
colombia <-  st_read(dsn = "~/Documents/Investigacion/Mapas Pemp/15_BOYACA/", layer = "depto")
setwd("~/Documents/Tesis/Referencias precios rigidos/Codigos R NPKC/")
coslab <- read_excel(paste("maptes.xlsx",sep=""),6)


colombia33 <- colombia %>% 
  left_join(coslab)


#colombia33[is.na(colombia33)] <- 0

colombia33ric <- colombia33 %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                           st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                              2))
colombia33 <- cbind(colombia33, st_coordinates(st_centroid(colombia33$geometry)))#importante

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=bac), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=forw), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()


ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=cm), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()

ggplot(colombia33)+ # tamaño 1000*800
  geom_sf(aes(fill=theta), show.legend = FALSE)+  xlab("") + ylab("") + 
  geom_text_repel(colombia33ric, mapping = aes(coords_x, coords_y, label = NOMBRE_DPT), size = 2, min.segment.length = 0) +
  scale_fill_gradientn(colours = plasma(4, alpha = 1, begin = 0.3, end = 1, direction = 1))+
  coord_sf(crs = st_crs(colombia33ric), datum = NA)+
  theme_minimal()



###################################################
#####################Grafica de dispersión de la inflación y los costos marginales

cat("\f")
rm(list = ls())
ciclos <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos con filtros/MesesCorridoCol2.dta")
names(ciclos)

ciclos2<- data.frame(ciclos[,c(51,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98)])
names(ciclos2)

ciclos3<- data.frame(ciclos[,c(27:36,38:49,51)])


ciclos2$sdcm = apply (ciclos2[ , 2:25], 1, sd)

ciclos3$sdinf = apply (ciclos3[ , 1:22], 1, sd)

costomsd<- data.frame(ciclos2[,c(26,1)])
infsd<- data.frame(ciclos3[,c(24,23)])


meses <- c("01-01-10","02-01-10","03-01-10","04-01-10","05-01-10","06-01-10","07-01-10","08-01-10","09-01-10","10-01-10","11-01-10","12-01-10",
           "01-01-11","02-01-11","03-01-11","04-01-11","05-01-11","06-01-11","07-01-11","08-01-11","09-01-11","10-01-11","11-01-11","12-01-11",
           "01-01-12","02-01-12","03-01-12","04-01-12","05-01-12","06-01-12","07-01-12","08-01-12","09-01-12","10-01-12","11-01-12","12-01-12",
           "01-01-13","02-01-13","03-01-13","04-01-13","05-01-13","06-01-13","07-01-13","08-01-13","09-01-13","10-01-13","11-01-13","12-01-13",
           "01-01-14","02-01-14","03-01-14","04-01-14","05-01-14","06-01-14","07-01-14","08-01-14","09-01-14","10-01-14","11-01-14","12-01-14",
           "01-01-15","02-01-15","03-01-15","04-01-15","05-01-15","06-01-15","07-01-15","08-01-15","09-01-15","10-01-15","11-01-15","12-01-15",
           "01-01-16","02-01-16","03-01-16","04-01-16","05-01-16","06-01-16","07-01-16","08-01-16","09-01-16","10-01-16","11-01-16","12-01-16",
           "01-01-17","02-01-17","03-01-17","04-01-17","05-01-17","06-01-17","07-01-17","08-01-17","09-01-17","10-01-17","11-01-17","12-01-17",
           "01-01-18","02-01-18","03-01-18","04-01-18","05-01-18","06-01-18","07-01-18","08-01-18","09-01-18","10-01-18","11-01-18","12-01-18",
           "01-01-19","02-01-19","03-01-19","04-01-19","05-01-19","06-01-19","07-01-19","08-01-19","09-01-19","10-01-19","11-01-19","12-01-19")

meses <- data.frame(meses)

costomsd$date <- as.Date(meses$meses, "%m-%d-%y")
costomsd$year <- year(costomsd$date)

costomsd2 <- rename(costomsd, c(cm=n))

costomsd3<- costomsd2 %>% gather("sdcm",key="Variables",value="valor",-year) 
costomsd4<-melt(costomsd3, id.vars = c("date","Variables","valor","year"))


ggplot(data = costomsd4, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1, show.legend = FALSE) + xlab("") + ylab("Dispersión en la brecha de los costos marginales (puntos porcentuales)") +
  scale_color_manual(values = c("#E7B800")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###################################

infsd$date <- as.Date(meses$meses, "%m-%d-%y")
infsd$year <- year(infsd$date)

infsd2 <- rename(infsd, c(cm=n))

infsd3<- infsd2 %>% gather("sdinf",key="Variables",value="valor",-year) 
infsd4<-melt(infsd3, id.vars = c("date","Variables","valor","year"))


ggplot(data = infsd4, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1, show.legend = FALSE) + xlab("") + ylab("Dispersión en la inflación (puntos porcentuales)") +
  scale_color_manual(values = c("#E7B800")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###########Dispersion por años############
cm14<- data.frame(ciclos[,c(1,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98)])
inf14<- data.frame(ciclos[,c(1,27:49)])

cm1<-filter(cm14,  mes==2010)
cm2<-filter(cm14,  mes==2011)
cm3<-filter(cm14,  mes==2012)
cm4<-filter(cm14,  mes==2013)
cm5<-filter(cm14,  mes==2014)

cmtot<-rbind(cm1,cm2,cm3,cm4,cm5)
cmtot2<- data.frame(cmtot[,c(2:25)])
cmtot3<-melt(cmtot2)
cmsd14<-summarise(cmtot3, sd= sd(value))

cmsd14

cm1<-filter(cm14,  mes==2015)
cm2<-filter(cm14,  mes==2016)
cm3<-filter(cm14,  mes==2017)
cm4<-filter(cm14,  mes==2018)
cm5<-filter(cm14,  mes==2019)

cmtot<-rbind(cm1,cm2,cm3,cm4,cm5)
cmtot2<- data.frame(cmtot[,c(2:25)])
cmtot3<-melt(cmtot2)
cmsd19<-summarise(cmtot3, sd= sd(value))

cmsd14
cmsd19



inf1<-filter(inf14,  mes==2010)
inf2<-filter(inf14,  mes==2011)
inf3<-filter(inf14,  mes==2012)
inf4<-filter(inf14,  mes==2013)
inf5<-filter(inf14,  mes==2014)

inftot<-rbind(inf1,inf2,inf3,inf4,inf5)
inftot2<- data.frame(inftot[,c(2:24)])
inftot3<-melt(inftot2)
infsd14<-summarise(inftot3, sd= sd(value))
infsd14

inf1<-filter(inf14,  mes==2015)
inf2<-filter(inf14,  mes==2016)
inf3<-filter(inf14,  mes==2017)
inf4<-filter(inf14,  mes==2018)
inf5<-filter(inf14,  mes==2019)

inftot<-rbind(inf1,inf2,inf3,inf4)
inftot2<- data.frame(inftot[,c(2:24)])
inftot3<-melt(inftot2)
infsd19<-summarise(inftot3, sd= sd(value))

cmsd14
cmsd19
infsd14
infsd19



##########################Plano cartesiano de los coeficientes###########
setwd("~/Documents/Tesis/Referencias precios rigidos/Codigos R NPKC/")
coef <- read_excel(paste("maptes.xlsx",sep=""),7)


ggplot(coef, aes(x= bac, y = cm)) + 
  geom_point(color = "yellow3", size = 3) +  labs( x= expression(gamma[b]), y= expression(lambda)) + 
  geom_label_repel(aes(label = depar),
box.padding   = 0.35, 
point.padding = 0.5,
segment.color = 'grey50')+  theme_bw()

ggplot(coef, aes(x= forw, y = cm)) + 
  geom_point(color = "yellow3", size = 3) +  labs( x= expression(gamma[f]), y= expression(lambda)) + 
  geom_label_repel(aes(label = depar),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+  theme_bw()


