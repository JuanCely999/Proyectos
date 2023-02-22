##############################################################################
###Desagregacion bases de datos ingresos laborales desde GEIH##
#http://microdatos.dane.gov.co/index.php/catalog/207/get_microdata
######################Autor: Juan Pablo Cely#################################
###############################18-03-2020####################################

rm(list=ls())
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)
library(readxl)
#costolab <-proxypibtrim %>% mutate(paso1 = (hola +hola2) * 3) 


#Trimestres
setwd("~/Documents/Tesis/Descomposicion PIB y GMM/")
#mesesdatos son los meses corridos, los mesesdatosnormales son los normales
mesesdatos <- read_excel("Actualizacion/mesesdatosnormales.xlsx")
names(mesesdatos)
costolab <-mesesdatos %>% 
  mutate(antidiv = cbanti/anti)  %>% mutate(anticm = antidiv / (1-0.6)) %>%
  mutate(atladiv = cbatla/atla)  %>% mutate(atlacm = atladiv / (1-0.6)) %>%
  mutate(bogodiv = cbbogo/bogo)  %>% mutate(bogocm = bogodiv / (1-0.6)) %>%
  mutate(bolidiv = cbboli/boli)  %>% mutate(bolicm = bolidiv / (1-0.6)) %>%
  mutate(boyadiv = cbboya/boya)  %>% mutate(boyacm = boyadiv / (1-0.6)) %>%
  mutate(calddiv = cbcald/cald)  %>% mutate(caldcm = calddiv / (1-0.6)) %>%
  mutate(caqudiv = cbcaqu/caqu)  %>% mutate(caqucm = caqudiv / (1-0.6)) %>%
  mutate(caucdiv = cbcauc/cauc)  %>% mutate(cauccm = caucdiv / (1-0.6)) %>%
  mutate(cesadiv = cbcesa/cesa)  %>% mutate(cesacm = cesadiv / (1-0.6)) %>%
  mutate(corddiv = cbcord/cord)  %>% mutate(cordcm = corddiv / (1-0.6)) %>%
  mutate(cunddiv = cbcund/cund)  %>% mutate(cundcm = cunddiv / (1-0.6)) %>%
  mutate(chocdiv = cbchoc/choc)  %>% mutate(choccm = chocdiv / (1-0.6)) %>%
  mutate(huildiv = cbhuil/huil)  %>% mutate(huilcm = huildiv / (1-0.6)) %>%
  mutate(laguadiv = cblagua/lagua)  %>% mutate(laguacm = laguadiv / (1-0.6)) %>%
  mutate(magddiv = cbmagd/magd)  %>% mutate(magdcm = magddiv / (1-0.6)) %>%
  mutate(metadiv = cbmeta/meta)  %>% mutate(metacm = metadiv / (1-0.6)) %>%
  mutate(naridiv = cbnari/nari)  %>% mutate(naricm = naridiv / (1-0.6)) %>%
  mutate(nortdiv = cbnort/nort)  %>% mutate(nortcm = nortdiv / (1-0.6)) %>%
  mutate(quindiv = cbquin/quin)  %>% mutate(quincm = quindiv / (1-0.6)) %>%
  mutate(risadiv = cbrisa/risa)  %>% mutate(risacm = risadiv / (1-0.6)) %>%
  mutate(santdiv = cbsant/sant)  %>% mutate(santcm = santdiv / (1-0.6)) %>%
  mutate(sucrdiv = cbsucr/sucr)  %>% mutate(sucrcm = sucrdiv / (1-0.6)) %>%
  mutate(tolidiv = cbtoli/toli)  %>% mutate(tolicm = tolidiv / (1-0.6)) %>%
  mutate(valldiv = cbvall/vall)  %>% mutate(vallcm = valldiv / (1-0.6)) %>%
  mutate(colombiadiv = cbcolombia/colombia)  %>% mutate(colombiacm = colombiadiv / (1-0.6))
  
names(costolab)
costolab2<-data.frame(costolab[,c(52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100)])
#costolabmensual.csv es el resultadoo del costo laboral de los meses corridos
write.csv(costolab2, file = "costolabmensual3.csv")

  
###########################TRIMESTRES#######################################
###########################ORIGINAL#######################################

rm(list=ls())


trimestresoriginal <- read_excel("Actualizacion/trimestresoriginal.xlsx")
names(trimestresoriginal)
costolab <-trimestresoriginal %>% 
  mutate(antidiv = cbanti/anti)  %>% mutate(anticm = antidiv / (1-0.6)) %>%
  mutate(atladiv = cbatla/atla)  %>% mutate(atlacm = atladiv / (1-0.6)) %>%
  mutate(bogodiv = cbbogo/bogo)  %>% mutate(bogocm = bogodiv / (1-0.6)) %>%
  mutate(bolidiv = cbboli/boli)  %>% mutate(bolicm = bolidiv / (1-0.6)) %>%
  mutate(boyadiv = cbboya/boya)  %>% mutate(boyacm = boyadiv / (1-0.6)) %>%
  mutate(calddiv = cbcald/cald)  %>% mutate(caldcm = calddiv / (1-0.6)) %>%
  mutate(caqudiv = cbcaqu/caqu)  %>% mutate(caqucm = caqudiv / (1-0.6)) %>%
  mutate(caucdiv = cbcauc/cauc)  %>% mutate(cauccm = caucdiv / (1-0.6)) %>%
  mutate(cesadiv = cbcesa/cesa)  %>% mutate(cesacm = cesadiv / (1-0.6)) %>%
  mutate(corddiv = cbcord/cord)  %>% mutate(cordcm = corddiv / (1-0.6)) %>%
  mutate(cunddiv = cbcund/cund)  %>% mutate(cundcm = cunddiv / (1-0.6)) %>%
  mutate(chocdiv = cbchoc/choc)  %>% mutate(choccm = chocdiv / (1-0.6)) %>%
  mutate(huildiv = cbhuil/huil)  %>% mutate(huilcm = huildiv / (1-0.6)) %>%
  mutate(laguadiv = cblagua/lagua)  %>% mutate(laguacm = laguadiv / (1-0.6)) %>%
  mutate(magddiv = cbmagd/magd)  %>% mutate(magdcm = magddiv / (1-0.6)) %>%
  mutate(metadiv = cbmeta/meta)  %>% mutate(metacm = metadiv / (1-0.6)) %>%
  mutate(naridiv = cbnari/nari)  %>% mutate(naricm = naridiv / (1-0.6)) %>%
  mutate(nortdiv = cbnort/nort)  %>% mutate(nortcm = nortdiv / (1-0.6)) %>%
  mutate(quindiv = cbquin/quin)  %>% mutate(quincm = quindiv / (1-0.6)) %>%
  mutate(risadiv = cbrisa/risa)  %>% mutate(risacm = risadiv / (1-0.6)) %>%
  mutate(santdiv = cbsant/sant)  %>% mutate(santcm = santdiv / (1-0.6)) %>%
  mutate(sucrdiv = cbsucr/sucr)  %>% mutate(sucrcm = sucrdiv / (1-0.6)) %>%
  mutate(tolidiv = cbtoli/toli)  %>% mutate(tolicm = tolidiv / (1-0.6)) %>%
  mutate(valldiv = cbvall/vall)  %>% mutate(vallcm = valldiv / (1-0.6)) %>%
  mutate(colombiadiv = cbcolombia/colombia)  %>% mutate(colombiacm = colombiadiv / (1-0.6))

names(costolab)
costolab2<-data.frame(costolab[,c(52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100)])
write.csv(costolab2, file = "costolabtrimoriginal.csv")

###########################TRIMESTRES#######################################
###########################REPLICA#######################################

rm(list=ls())


trimestres2 <- read_excel("Actualizacion/trimestres2.xlsx")
names(trimestres2)
costolab <-trimestres2 %>% 
  mutate(antidiv = cbanti/anti)  %>% mutate(anticm = antidiv / (1-0.6)) %>%
  mutate(atladiv = cbatla/atla)  %>% mutate(atlacm = atladiv / (1-0.6)) %>%
  mutate(bogodiv = cbbogo/bogo)  %>% mutate(bogocm = bogodiv / (1-0.6)) %>%
  mutate(bolidiv = cbboli/boli)  %>% mutate(bolicm = bolidiv / (1-0.6)) %>%
  mutate(boyadiv = cbboya/boya)  %>% mutate(boyacm = boyadiv / (1-0.6)) %>%
  mutate(calddiv = cbcald/cald)  %>% mutate(caldcm = calddiv / (1-0.6)) %>%
  mutate(caqudiv = cbcaqu/caqu)  %>% mutate(caqucm = caqudiv / (1-0.6)) %>%
  mutate(caucdiv = cbcauc/cauc)  %>% mutate(cauccm = caucdiv / (1-0.6)) %>%
  mutate(cesadiv = cbcesa/cesa)  %>% mutate(cesacm = cesadiv / (1-0.6)) %>%
  mutate(corddiv = cbcord/cord)  %>% mutate(cordcm = corddiv / (1-0.6)) %>%
  mutate(cunddiv = cbcund/cund)  %>% mutate(cundcm = cunddiv / (1-0.6)) %>%
  mutate(chocdiv = cbchoc/choc)  %>% mutate(choccm = chocdiv / (1-0.6)) %>%
  mutate(huildiv = cbhuil/huil)  %>% mutate(huilcm = huildiv / (1-0.6)) %>%
  mutate(laguadiv = cblagua/lagua)  %>% mutate(laguacm = laguadiv / (1-0.6)) %>%
  mutate(magddiv = cbmagd/magd)  %>% mutate(magdcm = magddiv / (1-0.6)) %>%
  mutate(metadiv = cbmeta/meta)  %>% mutate(metacm = metadiv / (1-0.6)) %>%
  mutate(naridiv = cbnari/nari)  %>% mutate(naricm = naridiv / (1-0.6)) %>%
  mutate(nortdiv = cbnort/nort)  %>% mutate(nortcm = nortdiv / (1-0.6)) %>%
  mutate(quindiv = cbquin/quin)  %>% mutate(quincm = quindiv / (1-0.6)) %>%
  mutate(risadiv = cbrisa/risa)  %>% mutate(risacm = risadiv / (1-0.6)) %>%
  mutate(santdiv = cbsant/sant)  %>% mutate(santcm = santdiv / (1-0.6)) %>%
  mutate(sucrdiv = cbsucr/sucr)  %>% mutate(sucrcm = sucrdiv / (1-0.6)) %>%
  mutate(tolidiv = cbtoli/toli)  %>% mutate(tolicm = tolidiv / (1-0.6)) %>%
  mutate(valldiv = cbvall/vall)  %>% mutate(vallcm = valldiv / (1-0.6)) %>%
  mutate(colombiadiv = cbcolombia/colombia)  %>% mutate(colombiacm = colombiadiv / (1-0.6))

names(costolab)
costolab2<-data.frame(costolab[,c(52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100)])
write.csv(costolab2, file = "costolabtrimrepli.csv")

  
  
