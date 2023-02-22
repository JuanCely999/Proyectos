##############################################################################
#                  Tesis:Proxy para calcular el PIB TRIMESTRAL              #
#                            Autor: Juan Pablo Cely                         #
###############################19-03-2020####################################
library(tempdisagg)
library(dplyr)
#help(tempdisagg)
library(readxl)
# Tenener en cuenta link  
#http://focoeconomico.org/2011/03/20/acerca-de-las-rigideces-de-precios/
#install.packages("tempdisagg")
setwd("~/Documents/Tesis/Descomposicion PIB y GMM/")

#####METODO CONTINUANDO LA TENDENCIA DE LOS TRIMESTRES DE COLOMBIA
#opcion 1

med2 <- td(med ~ 1 + nacional , conversion = "average",to = "quarterly")
opcion1med=data.frame(med3=predict(med2))

#opcion 1
nacional <- 
  ts(PYcolombia$colombia,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 4)
bog2 <- td(bog ~ 1 + nacional , conversion = "average",to = "quarterly")
opcion1bog=data.frame(bog3=predict(bog2))

#opcion 1
cal2 <- td(cal ~ 1 + nacional , conversion = "average",to = "quarterly")
opcion1cal=data.frame(cal3=predict(cal2))

#opcion 1
bar2 <- td(bar ~ 1 + nacional , conversion = "average",to = "quarterly")
opcion1bar=data.frame(bar3=predict(bar2))

#opcion 1
arm2 <- td(arm ~ 1 + nacional , conversion = "average",to = "quarterly")
opcion1arm=data.frame(arm3=predict(arm2))





#---------------------------INICIO--------------
rm(list=ls())

pib <- read_excel("PIB/pib.xlsx")

#https://rdrr.io/cran/tempdisagg/src/demo/tempdisagg.R

########################Antioquia############
anti2 <- 
  ts(pib$anti,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
anti3 <- td(anti2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
antimes=data.frame(anti=predict(anti3))

#trimestre
anti5 <- td(anti2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
antitrim=data.frame(anti=predict(anti5))


########################Atlantico############
atla2 <- 
  ts(pib$atla,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
atla3 <- td(atla2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
atlames=data.frame(atla=predict(atla3))

#trimestre
atla5 <- td(atla2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
atlatrim=data.frame(atla=predict(atla5))



########################Bogota############
bogo2 <- 
  ts(pib$bogo,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
bogo3 <- td(bogo2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
bogomes=data.frame(bogo=predict(bogo3))

#trimestre
bogo5 <- td(bogo2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
bogotrim=data.frame(bogo=predict(bogo5))




########################Bolivar############
boli2 <- 
  ts(pib$boli,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
boli3 <- td(boli2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
bolimes=data.frame(boli=predict(boli3))

#trimestre
boli5 <- td(boli2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
bolitrim=data.frame(boli=predict(boli5))


########################Boyaca############
boya2 <- 
  ts(pib$boya,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
boya3 <- td(boya2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
boyames=data.frame(boya=predict(boya3))

#trimestre
boya5 <- td(boya2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
boyatrim=data.frame(boya=predict(boya5))



########################Caldas############
cald2 <- 
  ts(pib$cald,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
cald3 <- td(cald2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
caldmes=data.frame(cald=predict(cald3))

#trimestre
cald5 <- td(cald2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
caldtrim=data.frame(cald=predict(cald5))


########################Caqueta############
caqu2 <- 
  ts(pib$caqu,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
caqu3 <- td(caqu2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
caqumes=data.frame(caqu=predict(caqu3))

#trimestre
caqu5 <- td(caqu2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
caqutrim=data.frame(caqu=predict(caqu5))



########################Cauca############
cauc2 <- 
  ts(pib$cauc,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
cauc3 <- td(cauc2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
caucmes=data.frame(cauc=predict(cauc3))

#trimestre
cauc5 <- td(cauc2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
cauctrim=data.frame(cauc=predict(cauc5))


########################Cesar############
cesa2 <- 
  ts(pib$cesa,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
cesa3 <- td(cesa2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
cesames=data.frame(cesa=predict(cesa3))

#trimestre
cesa5 <- td(cesa2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
cesatrim=data.frame(cesa=predict(cesa5))


########################Cordoba############
cord2 <- 
  ts(pib$cord,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
cord3 <- td(cord2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
cordmes=data.frame(cord=predict(cord3))

#trimestre
cord5 <- td(cord2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
cordtrim=data.frame(cord=predict(cord5))


########################Cundinamarca############
cund2 <- 
  ts(pib$cund,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
cund3 <- td(cund2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
cundmes=data.frame(cund=predict(cund3))

#trimestre
cund5 <- td(cund2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
cundtrim=data.frame(cund=predict(cund5))

########################Choco############
choc2 <- 
  ts(pib$choc,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
choc3 <- td(choc2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
chocmes=data.frame(choc=predict(choc3))

#trimestre
choc5 <- td(choc2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
choctrim=data.frame(choc=predict(choc5))


########################Huila############
huil2 <- 
  ts(pib$huil,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
huil3 <- td(huil2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
huilmes=data.frame(huil=predict(huil3))

#trimestre
huil5 <- td(huil2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
huiltrim=data.frame(huil=predict(huil5))


########################La Guajira############
lagua2 <- 
  ts(pib$lagua,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
lagua3 <- td(lagua2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
laguames=data.frame(lagua=predict(lagua3))

#trimestre
lagua5 <- td(lagua2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
laguatrim=data.frame(lagua=predict(lagua5))


########################Magdalena############
magd2 <- 
  ts(pib$magd,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
magd3 <- td(magd2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
magdmes=data.frame(magd=predict(magd3))

#trimestre
magd5 <- td(magd2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
magdtrim=data.frame(magd=predict(magd5))

########################Meta############
meta2 <- 
  ts(pib$meta,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
meta3 <- td(meta2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
metames=data.frame(meta=predict(meta3))

#trimestre
meta5 <- td(meta2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
metatrim=data.frame(meta=predict(meta5))


########################Nariño############
nari2 <- 
  ts(pib$nari,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
nari3 <- td(nari2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
narimes=data.frame(nari=predict(nari3))

#trimestre
nari5 <- td(nari2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
naritrim=data.frame(nari=predict(nari5))

########################Norte de santander############
nort2 <- 
  ts(pib$nort,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
nort3 <- td(nort2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
nortmes=data.frame(nort=predict(nort3))

#trimestre
nort5 <- td(nort2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
norttrim=data.frame(nort=predict(nort5))

########################Quindio############
quin2 <- 
  ts(pib$quin,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
quin3 <- td(quin2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
quinmes=data.frame(quin=predict(quin3))

#trimestre
quin5 <- td(quin2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
quintrim=data.frame(quin=predict(quin5))

########################Risaralda############
risa2 <- 
  ts(pib$risa,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
risa3 <- td(risa2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
risames=data.frame(risa=predict(risa3))

#trimestre
risa5 <- td(risa2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
risatrim=data.frame(risa=predict(risa5))



########################Santander############
sant2 <- 
  ts(pib$sant,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
sant3 <- td(sant2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
santmes=data.frame(sant=predict(sant3))

#trimestre
sant5 <- td(sant2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
santtrim=data.frame(sant=predict(sant5))


########################Sucre############
sucr2 <- 
  ts(pib$sucr,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
sucr3 <- td(sucr2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
sucrmes=data.frame(sucr=predict(sucr3))

#trimestre
sucr5 <- td(sucr2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
sucrtrim=data.frame(sucr=predict(sucr5))



########################Tolima############
toli2 <- 
  ts(pib$toli,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
toli3 <- td(toli2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
tolimes=data.frame(toli=predict(toli3))

#trimestre
toli5 <- td(toli2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
tolitrim=data.frame(toli=predict(toli5))

########################Valle del Cauca############
vall2 <- 
  ts(pib$vall,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 1)

#mes
vall3 <- td(vall2 ~ 1, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
vallmes=data.frame(vall=predict(vall3))

#trimestre
vall5 <- td(vall2 ~ 1, conversion = "average",to = "quarterly", method = "chow-lin-maxlog")
valltrim=data.frame(vall=predict(vall5))






########################Colombia############
PYcolombia <- read_excel("PIB/PYcolombia.xlsx")

colombia <- 
  ts(PYcolombia$colombia,   #La series es mensual, 12 puntos de datos.   
     start=2009,frequency = 4)

#mes
colombia2 <- td(colombia ~ 4, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
colombiames=data.frame(colombia3=predict(colombia2))

mesespib<-cbind(antimes,atlames,bogomes,bolimes,boyames,caldmes,caqumes,caucmes,cesames,cordmes,
                  cundmes,chocmes,huilmes,laguames,magdmes,metames,narimes,nortmes,quinmes,risames,
                  santmes,sucrmes,tolimes,vallmes,colombiames)


trimespib<-cbind(antitrim,atlatrim,bogotrim,bolitrim,boyatrim,caldtrim,caqutrim,cauctrim,cesatrim,cordtrim,
                cundtrim,choctrim,huiltrim,laguatrim,magdtrim,metatrim,naritrim,norttrim,quintrim,risatrim,
                santtrim,sucrtrim,tolitrim,valltrim,PYcolombia)


write.csv(mesespib, file = "mesespib.csv")
write.csv(trimespib, file = "trimespib.csv")
