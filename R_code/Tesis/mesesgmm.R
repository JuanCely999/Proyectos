#########################################################################################
#############                   BANCO DE LA REP?BLICA                  ##################
#############            Chap. 15 Introductory Econometrics             ##################
#########################################################################################
cat("\f")
rm(list = ls())
library(stats)
library(openxlsx)
library(car)
library(AER)
library(dynlm)
library(gmm)
library(haven)


######PRUEBA NKPC para Antioquia en meses (2010)

nacional <- read_dta("Actualizacion/Bases de datos con filtros/MesesCorrido.dta")
#nacional <- read_dta("Actualizacion/Bases de datos con filtros/Mesesnormales.dta")

names(nacional)



##################################################################
#################anti
##################################################################

anti<- data.frame(nacional[,c(52,27)])
anti<-na.omit(anti)
anti <- 
  ts(anti,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(anti[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(anti[,2],-3))

cm<-cbind(anti[,1])
for (i in 1:7) cm<-cbind(cm,lag(anti[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(anti[,2],3))

#ipc2<-lag(nacional$anticm,3)


anti1<-cbind(ipca3,ipca2,cm,anti[,1:2])
anti1<-na.omit(anti1)

colnames(anti1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmanti<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =anti1, vcov="HAC")

coeftest(gmmanti)
#coef(summary(gmmanti))
#summary(gmmanti)







##################################################################
#################atla
##################################################################

atla<- data.frame(nacional[,c(54,28)])
atla<-na.omit(atla)
atla <- 
  ts(atla,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(atla[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(atla[,2],-3))

cm<-cbind(atla[,1])
for (i in 1:7) cm<-cbind(cm,lag(atla[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(atla[,2],3))

#ipc2<-lag(nacional$atlacm,3)


atla1<-cbind(ipca3,ipca2,cm,atla[,1:2])
atla1<-na.omit(atla1)

colnames(atla1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmatla<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =atla1, vcov="HAC")

coeftest(gmmatla)
#coef(summary(gmmatla))
#summary(gmmatla)



##################################################################
#################bogo
##################################################################

bogo<- data.frame(nacional[,c(56,29)])
bogo<-na.omit(bogo)
bogo <- 
  ts(bogo,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(bogo[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(bogo[,2],-3))

cm<-cbind(bogo[,1])
for (i in 1:7) cm<-cbind(cm,lag(bogo[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(bogo[,2],3))

#ipc2<-lag(nacional$bogocm,3)


bogo1<-cbind(ipca3,ipca2,cm,bogo[,1:2])
bogo1<-na.omit(bogo1)

colnames(bogo1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmbogo<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =bogo1, vcov="HAC")

coeftest(gmmbogo)
#coef(summary(gmmbogo))
#summary(gmmbogo)






#################################################################
#################boli
##################################################################

boli<- data.frame(nacional[,c(58,30)])
boli<-na.omit(boli)
boli <- 
  ts(boli,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(boli[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(boli[,2],-3))

cm<-cbind(boli[,1])
for (i in 1:7) cm<-cbind(cm,lag(boli[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(boli[,2],3))

#ipc2<-lag(nacional$bolicm,3)


boli1<-cbind(ipca3,ipca2,cm,boli[,1:2])
boli1<-na.omit(boli1)

colnames(boli1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmboli<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =boli1, vcov="HAC")

coeftest(gmmboli)
#coef(summary(gmmboli))
#summary(gmmboli)




#################################################################
#################boya
##################################################################

boya<- data.frame(nacional[,c(60,31)])
boya<-na.omit(boya)
boya <- 
  ts(boya,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(boya[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(boya[,2],-3))

cm<-cbind(boya[,1])
for (i in 1:7) cm<-cbind(cm,lag(boya[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(boya[,2],3))

#ipc2<-lag(nacional$boyacm,3)


boya1<-cbind(ipca3,ipca2,cm,boya[,1:2])
boya1<-na.omit(boya1)

colnames(boya1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmboya<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =boya1, vcov="HAC")

coeftest(gmmboya)
#coef(summary(gmmboya))
#summary(gmmboya)



#################################################################
#################cald
##################################################################

cald<- data.frame(nacional[,c(62,32)])
cald<-na.omit(cald)
cald <- 
  ts(cald,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(cald[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(cald[,2],-3))

cm<-cbind(cald[,1])
for (i in 1:7) cm<-cbind(cm,lag(cald[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(cald[,2],3))

#ipc2<-lag(nacional$caldcm,3)


cald1<-cbind(ipca3,ipca2,cm,cald[,1:2])
cald1<-na.omit(cald1)

colnames(cald1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcald<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =cald1, vcov="HAC")

coeftest(gmmcald)
#coef(summary(gmmcald))
#summary(gmmcald)



#################################################################
#################caqu
##################################################################

caqu<- data.frame(nacional[,c(64,33)])
caqu<-na.omit(caqu)
caqu <- 
  ts(caqu,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(caqu[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(caqu[,2],-3))

cm<-cbind(caqu[,1])
for (i in 1:7) cm<-cbind(cm,lag(caqu[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(caqu[,2],3))

#ipc2<-lag(nacional$caqucm,3)


caqu1<-cbind(ipca3,ipca2,cm,caqu[,1:2])
caqu1<-na.omit(caqu1)

colnames(caqu1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcaqu<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =caqu1, vcov="HAC")

coeftest(gmmcaqu)
#coef(summary(gmmcaqu))
#summary(gmmcaqu)



#################################################################
#################cauc
##################################################################

cauc<- data.frame(nacional[,c(66,34)])
cauc<-na.omit(cauc)
cauc <- 
  ts(cauc,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(cauc[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(cauc[,2],-3))

cm<-cbind(cauc[,1])
for (i in 1:7) cm<-cbind(cm,lag(cauc[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(cauc[,2],3))

#ipc2<-lag(nacional$cauccm,3)


cauc1<-cbind(ipca3,ipca2,cm,cauc[,1:2])
cauc1<-na.omit(cauc1)

colnames(cauc1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcauc<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =cauc1, vcov="HAC")

coeftest(gmmcauc)
#coef(summary(gmmcauc))
#summary(gmmcauc)


#################################################################
#################cesa
##################################################################

cesa<- data.frame(nacional[,c(68,35)])
cesa<-na.omit(cesa)
cesa <- 
  ts(cesa,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(cesa[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(cesa[,2],-3))

cm<-cbind(cesa[,1])
for (i in 1:7) cm<-cbind(cm,lag(cesa[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(cesa[,2],3))

#ipc2<-lag(nacional$cesacm,3)


cesa1<-cbind(ipca3,ipca2,cm,cesa[,1:2])
cesa1<-na.omit(cesa1)

colnames(cesa1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcesa<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =cesa1, vcov="HAC")

coeftest(gmmcesa)
#coef(summary(gmmcesa))
#summary(gmmcesa)


#################################################################
#################cord
##################################################################

cord<- data.frame(nacional[,c(70,36)])
cord<-na.omit(cord)
cord <- 
  ts(cord,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(cord[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(cord[,2],-3))

cm<-cbind(cord[,1])
for (i in 1:7) cm<-cbind(cm,lag(cord[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(cord[,2],3))

#ipc2<-lag(nacional$cordcm,3)


cord1<-cbind(ipca3,ipca2,cm,cord[,1:2])
cord1<-na.omit(cord1)

colnames(cord1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcord<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =cord1, vcov="HAC")

coeftest(gmmcord)
#coef(summary(gmmcord))
#summary(gmmcord)


#################################################################
#################cund
##################################################################

cund<- data.frame(nacional[,c(72,29)])
cund<-na.omit(cund)
cund <- 
  ts(cund,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(cund[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(cund[,2],-3))

cm<-cbind(cund[,1])
for (i in 1:7) cm<-cbind(cm,lag(cund[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(cund[,2],3))

#ipc2<-lag(nacional$cundcm,3)


cund1<-cbind(ipca3,ipca2,cm,cund[,1:2])
cund1<-na.omit(cund1)

colnames(cund1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcund<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =cund1, vcov="HAC")

coeftest(gmmcund)
#coef(summary(gmmcund))
#summary(gmmcund)



#################################################################
#################choc
##################################################################

choc<- data.frame(nacional[,c(74,37)])
choc<-na.omit(choc)
choc <- 
  ts(choc,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(choc[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(choc[,2],-3))

cm<-cbind(choc[,1])
for (i in 1:7) cm<-cbind(cm,lag(choc[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(choc[,2],3))

#ipc2<-lag(nacional$choccm,3)


choc1<-cbind(ipca3,ipca2,cm,choc[,1:2])
choc1<-na.omit(choc1)

colnames(choc1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmchoc<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =choc1, vcov="HAC")

coeftest(gmmchoc)
#coef(summary(gmmchoc))
#summary(gmmchoc)



#################################################################
#################huil
##################################################################

huil<- data.frame(nacional[,c(76,38)])
huil<-na.omit(huil)
huil <- 
  ts(huil,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(huil[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(huil[,2],-3))

cm<-cbind(huil[,1])
for (i in 1:7) cm<-cbind(cm,lag(huil[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(huil[,2],3))

#ipc2<-lag(nacional$huilcm,3)


huil1<-cbind(ipca3,ipca2,cm,huil[,1:2])
huil1<-na.omit(huil1)

colnames(huil1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmhuil<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =huil1, vcov="HAC")

coeftest(gmmhuil)
#coef(summary(gmmhuil))
#summary(gmmhuil)



#################################################################
#################lagua
##################################################################

lagua<- data.frame(nacional[,c(78,39)])
lagua<-na.omit(lagua)
lagua <- 
  ts(lagua,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(lagua[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(lagua[,2],-3))

cm<-cbind(lagua[,1])
for (i in 1:7) cm<-cbind(cm,lag(lagua[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(lagua[,2],3))

#ipc2<-lag(nacional$laguacm,3)


lagua1<-cbind(ipca3,ipca2,cm,lagua[,1:2])
lagua1<-na.omit(lagua1)

colnames(lagua1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmlagua<-gmm(ipc~ipct_1+ipct1+cmt_1,
              ~ipct1+ipct_1+cmt_1,
              ~cmt_1,ipct_1+ipct1,data =lagua1, vcov="HAC")

coeftest(gmmlagua)
#coef(summary(gmmlagua))
#summary(gmmlagua)


#################################################################
#################magd
##################################################################

magd<- data.frame(nacional[,c(80,40)])
magd<-na.omit(magd)
magd <- 
  ts(magd,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(magd[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(magd[,2],-3))

cm<-cbind(magd[,1])
for (i in 1:7) cm<-cbind(cm,lag(magd[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(magd[,2],3))

#ipc2<-lag(nacional$magdcm,3)


magd1<-cbind(ipca3,ipca2,cm,magd[,1:2])
magd1<-na.omit(magd1)

colnames(magd1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmmagd<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =magd1, vcov="HAC")

coeftest(gmmmagd)
#coef(summary(gmmmagd))
#summary(gmmmagd)



#################################################################
#################meta
##################################################################

meta<- data.frame(nacional[,c(82,41)])
meta<-na.omit(meta)
meta <- 
  ts(meta,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(meta[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(meta[,2],-3))

cm<-cbind(meta[,1])
for (i in 1:7) cm<-cbind(cm,lag(meta[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(meta[,2],3))

#ipc2<-lag(nacional$metacm,3)


meta1<-cbind(ipca3,ipca2,cm,meta[,1:2])
meta1<-na.omit(meta1)

colnames(meta1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmmeta<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =meta1, vcov="HAC")

coeftest(gmmmeta)
#coef(summary(gmmmeta))
#summary(gmmmeta)


#################################################################
#################nari
##################################################################

nari<- data.frame(nacional[,c(84,42)])
nari<-na.omit(nari)
nari <- 
  ts(nari,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(nari[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(nari[,2],-3))

cm<-cbind(nari[,1])
for (i in 1:7) cm<-cbind(cm,lag(nari[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(nari[,2],3))

#ipc2<-lag(nacional$naricm,3)


nari1<-cbind(ipca3,ipca2,cm,nari[,1:2])
nari1<-na.omit(nari1)

colnames(nari1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmnari<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =nari1, vcov="HAC")

coeftest(gmmnari)
#coef(summary(gmmnari))
#summary(gmmnari)


#################################################################
#################nort
##################################################################

nort<- data.frame(nacional[,c(86,43)])
nort<-na.omit(nort)
nort <- 
  ts(nort,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(nort[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(nort[,2],-3))

cm<-cbind(nort[,1])
for (i in 1:7) cm<-cbind(cm,lag(nort[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(nort[,2],3))

#ipc2<-lag(nacional$nortcm,3)




#################################################################
#################quin
##################################################################

quin<- data.frame(nacional[,c(88,44)])
quin<-na.omit(quin)
quin <- 
  ts(quin,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(quin[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(quin[,2],-3))

cm<-cbind(quin[,1])
for (i in 1:7) cm<-cbind(cm,lag(quin[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(quin[,2],3))

#ipc2<-lag(nacional$quincm,3)


quin1<-cbind(ipca3,ipca2,cm,quin[,1:2])
quin1<-na.omit(quin1)

colnames(quin1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmquin<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =quin1, vcov="HAC")

coeftest(gmmquin)
#coef(summary(gmmquin))
#summary(gmmquin)


nort1<-cbind(ipca3,ipca2,cm,nort[,1:2])
nort1<-na.omit(nort1)

colnames(nort1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmnort<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =nort1, vcov="HAC")

coeftest(gmmnort)
#coef(summary(gmmnort))
#summary(gmmnort)



#################################################################
#################risa
##################################################################

risa<- data.frame(nacional[,c(90,45)])
risa<-na.omit(risa)
risa <- 
  ts(risa,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(risa[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(risa[,2],-3))

cm<-cbind(risa[,1])
for (i in 1:7) cm<-cbind(cm,lag(risa[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(risa[,2],3))

#ipc2<-lag(nacional$risacm,3)


risa1<-cbind(ipca3,ipca2,cm,risa[,1:2])
risa1<-na.omit(risa1)

colnames(risa1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmrisa<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =risa1, vcov="HAC")

coeftest(gmmrisa)
#coef(summary(gmmrisa))
#summary(gmmrisa)



#################################################################
#################sant
##################################################################

sant<- data.frame(nacional[,c(92,46)])
sant<-na.omit(sant)
sant <- 
  ts(sant,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(sant[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(sant[,2],-3))

cm<-cbind(sant[,1])
for (i in 1:7) cm<-cbind(cm,lag(sant[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(sant[,2],3))

#ipc2<-lag(nacional$santcm,3)


sant1<-cbind(ipca3,ipca2,cm,sant[,1:2])
sant1<-na.omit(sant1)

colnames(sant1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmsant<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =sant1, vcov="HAC")

coeftest(gmmsant)
#coef(summary(gmmsant))
#summary(gmmsant)


#################################################################
#################sucr
##################################################################

sucr<- data.frame(nacional[,c(94,47)])
sucr<-na.omit(sucr)
sucr <- 
  ts(sucr,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(sucr[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(sucr[,2],-3))

cm<-cbind(sucr[,1])
for (i in 1:7) cm<-cbind(cm,lag(sucr[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(sucr[,2],3))

#ipc2<-lag(nacional$sucrcm,3)


sucr1<-cbind(ipca3,ipca2,cm,sucr[,1:2])
sucr1<-na.omit(sucr1)

colnames(sucr1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmsucr<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =sucr1, vcov="HAC")

coeftest(gmmsucr)
#coef(summary(gmmsucr))
#summary(gmmsucr)



#################################################################
#################toli
##################################################################

toli<- data.frame(nacional[,c(96,48)])
toli<-na.omit(toli)
toli <- 
  ts(toli,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(toli[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(toli[,2],-3))

cm<-cbind(toli[,1])
for (i in 1:7) cm<-cbind(cm,lag(toli[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(toli[,2],3))

#ipc2<-lag(nacional$tolicm,3)


toli1<-cbind(ipca3,ipca2,cm,toli[,1:2])
toli1<-na.omit(toli1)

colnames(toli1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmtoli<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =toli1, vcov="HAC")

coeftest(gmmtoli)
#coef(summary(gmmtoli))
#summary(gmmtoli)



#################################################################
#################vall
##################################################################

vall<- data.frame(nacional[,c(98,49)])
vall<-na.omit(vall)
vall <- 
  ts(vall,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(vall[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(vall[,2],-3))

cm<-cbind(vall[,1])
for (i in 1:7) cm<-cbind(cm,lag(vall[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(vall[,2],3))

#ipc2<-lag(nacional$vallcm,3)


vall1<-cbind(ipca3,ipca2,cm,vall[,1:2])
vall1<-na.omit(vall1)

colnames(vall1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmvall<-gmm(ipc~ipct_1+ipct1+cmt_1,
             ~ipct1+ipct_1+cmt_1,
             ~cmt_1,ipct_1+ipct1,data =vall1, vcov="HAC")

coeftest(gmmvall)
#coef(summary(gmmvall))
#summary(gmmvall)



#################################################################
#################colombia
##################################################################

colombia<- data.frame(nacional[,c(100,50)])
colombia<-na.omit(colombia)
colombia <- 
  ts(colombia,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(colombia[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(colombia[,2],-3))

cm<-cbind(colombia[,1])
for (i in 1:7) cm<-cbind(cm,lag(colombia[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(colombia[,2],3))

#ipc2<-lag(nacional$colombiacm,3)


colombia1<-cbind(ipca3,ipca2,cm,colombia[,1:2])
colombia1<-na.omit(colombia1)

colnames(colombia1)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")

gmmcolombia<-gmm(ipc~ipct_1+ipct1+cmt_1,
                 ~ipct1+ipct_1+cmt_1,
                 ~cmt_1,ipct_1+ipct1,data =colombia1, vcov="HAC")

coeftest(gmmcolombia)
#coef(summary(gmmcolombia))
#summary(gmmcolombia)
coeftest(gmmanti)
coeftest(gmmatla)
coeftest(gmmbogo)
coeftest(gmmboli)
coeftest(gmmboya)
coeftest(gmmcald)
coeftest(gmmcaqu)
coeftest(gmmcauc)
coeftest(gmmcesa)
coeftest(gmmcord)
coeftest(gmmcund)
coeftest(gmmchoc)
coeftest(gmmhuil)
coeftest(gmmlagua)
coeftest(gmmmagd)
coeftest(gmmmeta)
coeftest(gmmnari)
coeftest(gmmnort)
coeftest(gmmquin)
coeftest(gmmrisa)
coeftest(gmmsant)
coeftest(gmmsucr)
coeftest(gmmtoli)
coeftest(gmmvall)
coeftest(gmmcolombia)

