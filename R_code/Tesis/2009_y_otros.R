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
#https://mauricioanderson.com/curso-r-tidyr/

#str(separate(data =  Cabecera,  col  =  fex_c_2011,   into =  c("urbana", " total", "urbana2", " total2", " total3"), sep  =  ","))

#Cabecera2 <- Cabecera %>% separate(`Cabecera...c.178..`, into = c("urbana", " total3"), sep = ". ")
#select(na.omit(concat.split.multiple(melt(Cabecera, id.vars="Cabecera...c.178.."), split.col="Cabecera...c.178..", sep=".", direction="long")), -time)
#Cabecera$fex_c_2011= as.factor(Cabecera$fex_c_2011)
#`Cabecera...Ocupados.(1)`<- data.frame(`Cabecera...Ocupados.(1)`[,c(175:178)])
#trim091<-cbind(enero09, Febrero09,Marzo09)
#trim0912 <-trim091 %>% mutate(sumatrim = suma) 
#names (trim091) = c("1", "2", "3", "4", "5", "6", "7")
setwd("Documents/Tesis/Bases de datos/Ingresos laboral")

#NUNCA UTILIZAR FORMATO csv O SI SE UTILIZA NO TRANSFORMARLAS EN VARIABLES NUMERICAS YA QUE CAMBIA SU VALOR

############################################################################
#                                      2009                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2009            #
rm(list=ls())

#Archivos
Cabecera <- read_sav("Ingresos laboral/2009/Enero/01/Cabecera - Ocupados (1).sav")
resto <- read_sav("Ingresos laboral/2009/Enero/01/Resto - Ocupados (1).sav")
area<- read_sav("Ingresos laboral/2009/Enero/01/탍ea - Ocupados  (1).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0901 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero09.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2009            #

#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/Febrero/02/Cabecera - Ocupados (2).sav")
resto<- read_sav("Ingresos laboral/2009/Febrero/02/Resto - Ocupados (2).sav")
area<- read_sav("Ingresos laboral/2009/Febrero/02/탍ea - Ocupados (2).sav")


#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0902 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero09.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2009           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2009/Marzo/03/Cabecera - Ocupados (3).sav")
resto <- read_sav("Ingresos laboral/2009/Marzo/03/Resto - Ocupados (3).sav")
area<- read_sav("Ingresos laboral/2009/Marzo/03/탍ea - Ocupados (3).sav")



#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0903 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/04/04/Cabecera - Ocupados (4).sav")
resto<- read_sav("Ingresos laboral/2009/04/04/Resto - Ocupados (4).sav")
area<- read_sav("Ingresos laboral/2009/04/04/Área - Ocupados (4).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0904 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/05/05/Cabecera - Ocupados (5).sav")
resto<- read_sav("Ingresos laboral/2009/05/05/Resto - Ocupados (5).sav")
area<- read_sav("Ingresos laboral/2009/05/05/Área - Ocupados (5).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0905 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/06/06/Cabecera - Ocupados (6).sav")
resto<- read_sav("Ingresos laboral/2009/06/06/Resto - Ocupados (6).sav")
area<- read_sav("Ingresos laboral/2009/06/06/Área - Ocupados (6).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0906 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/07/07/Cabecera - Ocupados (7(.sav")
resto<- read_sav("Ingresos laboral/2009/07/07/Resto - Ocupados (7).sav")
area<- read_sav("Ingresos laboral/2009/07/07/Área - Ocupados (7).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0907 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio09.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/08/08/Cabecera - Ocupados (8).sav")
resto<- read_sav("Ingresos laboral/2009/08/08/Resto - Ocupados (8).sav")
area<- read_sav("Ingresos laboral/2009/08/08/Área - Ocupados (8).sav")


#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0908 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto09.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/09/09/Cabecera - Ocupados (9).sav")
resto<- read_sav("Ingresos laboral/2009/09/09/Resto - Ocupados (9).sav")
area<- read_sav("Ingresos laboral/2009/09/09/Área - Ocupados (9).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0909 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre09.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/10/10/Cabecera - Ocupados (10).sav")
resto<- read_sav("Ingresos laboral/2009/10/10/Resto - Ocupados (10).sav")
area<- read_sav("Ingresos laboral/2009/10/10/Área - Ocupados (10).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0910 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/11/11/Cabecera - Ocupados (11).sav")
resto<- read_sav("Ingresos laboral/2009/11/11/Resto - Ocupados (11).sav")
area<- read_sav("Ingresos laboral/2009/11/11/Área - Ocupados (11).sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0911 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre09.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2009           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2009/12/12/Cabecera - Ocupados (12).sav")
resto<- read_sav("Ingresos laboral/2009/12/12/Resto - Ocupados (12).sav")
area<- read_sav("Ingresos laboral/2009/12/12/Área - Ocupados (12).sav")


#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = INGLABO * fex_c_2011)
cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = INGLABO * fex_c_2011) 
res <- resto3 %>% group_by(DPTO) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = INGLABO * fex_c_2011) 
are <- area3 %>% group_by(DPTO) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma0912 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre09.csv")







#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

setwd("Documents/Tesis/Bases de datos/")
rm(list=ls())

#           Suma por trimestres y años          #
#laboral3 <-hola %>% mutate(operacion = (hola +hola2) * 3) 
#2009
#ene
ene2009<- read.csv("enero09.csv")
ene2009<- data.frame(ene2009[,c(2:3)])
names(ene2009)<- c("DPTO","suma0901")
#feb
feb2009<- read.csv("Febrero09.csv")
feb2009<- data.frame(feb2009[,c(3)])
names(feb2009)<- c("suma0902")

#mar
mar2009<- read.csv("Marzo09.csv")
mar2009<- data.frame(mar2009[,c(3)])
names(mar2009)<- c("suma0903")

#abr
abr2009<- read.csv("Abril09.csv")
abr2009<- data.frame(abr2009[,c(3)])
names(abr2009)<- c("suma0904")

#may
may2009<- read.csv("Mayo09.csv")
may2009<- data.frame(may2009[,c(3)])
names(may2009)<- c("suma0905")

#jun
jun2009<- read.csv("Junio09.csv")
jun2009<- data.frame(jun2009[,c(3)])
names(jun2009)<- c("suma0906")

#jul
jul2009<- read.csv("Julio09.csv")
jul2009<- data.frame(jul2009[,c(3)])
names(jul2009)<- c("suma0907")

#ago
ago2009<- read.csv("Agosto09.csv")
ago2009<- data.frame(ago2009[,c(3)])
names(ago2009)<- c("suma0908")

#sep
sep2009<- read.csv("Septiembre09.csv")
sep2009<- data.frame(sep2009[,c(3)])
names(sep2009)<- c("suma0909")

#oct
oct2009<- read.csv("Octubre09.csv")
oct2009<- data.frame(oct2009[,c(3)])
names(oct2009)<- c("suma0910")

#nov
nov2009<- read.csv("Noviembre09.csv")
nov2009<- data.frame(nov2009[,c(3)])
names(nov2009)<- c("suma0911")

#dic
dic2009<- read.csv("Diciembre09.csv")
dic2009<- data.frame(dic2009[,c(3)])
names(dic2009)<- c("suma0912")


#Sumatoria por trimestres y años
union2009<-cbind(ene2009,feb2009,mar2009,abr2009,may2009,jun2009,jul2009,ago2009,sep2009,oct2009,nov2009,dic2009)
union2009 <-union2009 %>% mutate(l2009 = suma0901 + suma0902 + suma0903) %>% 
  mutate(ll2009 = suma0904 + suma0905 + suma0906) %>% 
  mutate(lll2009 = suma0907 + suma0908 + suma0909) %>% 
  mutate(llll2009 = suma0910 + suma0911 + suma0912)%>% 
  mutate(fin2009 = llll2009 +lll2009+ll2009+l2009)

trimestre2009<-data.frame(union2009[,c(1,14:17)])
write.csv(trimestre2009, file = "trim2009.csv")

ano2009<-data.frame(union2009[,c(1,18)])
write.csv(ano2009, file = "ano2009.csv")


rm(list=ls())
#2010
#ene
ene2010<- read.csv("enero2010.csv")
ene2010<- data.frame(ene2010[,c(2:3)])
names(ene2010)<- c("DPTO","suma201001")
#feb
feb2010<- read.csv("Febrero2010.csv")
feb2010<- data.frame(feb2010[,c(3)])
names(feb2010)<- c("suma201002")

#mar
mar2010<- read.csv("Marzo2010.csv")
mar2010<- data.frame(mar2010[,c(3)])
names(mar2010)<- c("suma201003")

#abr
abr2010<- read.csv("Abril2010.csv")
abr2010<- data.frame(abr2010[,c(3)])
names(abr2010)<- c("suma201004")

#may
may2010<- read.csv("Mayo2010.csv")
may2010<- data.frame(may2010[,c(3)])
names(may2010)<- c("suma201005")

#jun
jun2010<- read.csv("Junio2010.csv")
jun2010<- data.frame(jun2010[,c(3)])
names(jun2010)<- c("suma201006")

#jul
jul2010<- read.csv("Julio2010.csv")
jul2010<- data.frame(jul2010[,c(3)])
names(jul2010)<- c("suma201007")

#ago
ago2010<- read.csv("Agosto2010.csv")
ago2010<- data.frame(ago2010[,c(3)])
names(ago2010)<- c("suma201008")

#sep
sep2010<- read.csv("Septiembre2010.csv")
sep2010<- data.frame(sep2010[,c(3)])
names(sep2010)<- c("suma201009")

#oct
oct2010<- read.csv("Octubre2010.csv")
oct2010<- data.frame(oct2010[,c(3)])
names(oct2010)<- c("suma201010")

#nov
nov2010<- read.csv("Noviembre2010.csv")
nov2010<- data.frame(nov2010[,c(3)])
names(nov2010)<- c("suma201011")

#dic
dic2010<- read.csv("Diciembre2010.csv")
dic2010<- data.frame(dic2010[,c(3)])
names(dic2010)<- c("suma201012")


#Sumatoria por trimestres y años
union2010<-cbind(ene2010,feb2010,mar2010,abr2010,may2010,jun2010,jul2010,ago2010,sep2010,oct2010,nov2010,dic2010)
union2010 <-union2010 %>% mutate(l2010 = suma201001 + suma201002 + suma201003) %>% 
  mutate(ll2010 = suma201004 + suma201005 + suma201006) %>% 
  mutate(lll2010 = suma201007 + suma201008 + suma201009) %>% 
  mutate(llll2010 = suma201010 + suma201011 + suma201012)%>% 
  mutate(fin2010 = llll2010 +lll2010+ll2010+l2010)

trimestre2010<-data.frame(union2010[,c(14:17)])
write.csv(trimestre2010, file = "trim2010.csv")

ano2010<-data.frame(union2010[,c(18)])
write.csv(ano2010, file = "ano2010.csv")





rm(list=ls())
#2011
#ene
ene2011<- read.csv("enero2011.csv")
ene2011<- data.frame(ene2011[,c(2:3)])
names(ene2011)<- c("DPTO","suma201101")
#feb
feb2011<- read.csv("Febrero2011.csv")
feb2011<- data.frame(feb2011[,c(3)])
names(feb2011)<- c("suma201102")

#mar
mar2011<- read.csv("Marzo2011.csv")
mar2011<- data.frame(mar2011[,c(3)])
names(mar2011)<- c("suma201103")

#abr
abr2011<- read.csv("Abril2011.csv")
abr2011<- data.frame(abr2011[,c(3)])
names(abr2011)<- c("suma201104")

#may
may2011<- read.csv("Mayo2011.csv")
may2011<- data.frame(may2011[,c(3)])
names(may2011)<- c("suma201105")

#jun
jun2011<- read.csv("Junio2011.csv")
jun2011<- data.frame(jun2011[,c(3)])
names(jun2011)<- c("suma201106")

#jul
jul2011<- read.csv("Julio2011.csv")
jul2011<- data.frame(jul2011[,c(3)])
names(jul2011)<- c("suma201107")

#ago
ago2011<- read.csv("Agosto2011.csv")
ago2011<- data.frame(ago2011[,c(3)])
names(ago2011)<- c("suma201108")

#sep
sep2011<- read.csv("Septiembre2011.csv")
sep2011<- data.frame(sep2011[,c(3)])
names(sep2011)<- c("suma201109")

#oct
oct2011<- read.csv("Octubre2011.csv")
oct2011<- data.frame(oct2011[,c(3)])
names(oct2011)<- c("suma201110")

#nov
nov2011<- read.csv("Noviembre2011.csv")
nov2011<- data.frame(nov2011[,c(3)])
names(nov2011)<- c("suma201111")

#dic
dic2011<- read.csv("Diciembre2011.csv")
dic2011<- data.frame(dic2011[,c(3)])
names(dic2011)<- c("suma201112")


#Sumatoria por trimestres y años
union2011<-cbind(ene2011,feb2011,mar2011,abr2011,may2011,jun2011,jul2011,ago2011,sep2011,oct2011,nov2011,dic2011)
union2011 <-union2011 %>% mutate(l2011 = suma201101 + suma201102 + suma201103) %>% 
  mutate(ll2011 = suma201104 + suma201105 + suma201106) %>% 
  mutate(lll2011 = suma201107 + suma201108 + suma201109) %>% 
  mutate(llll2011 = suma201110 + suma201111 + suma201112)%>% 
  mutate(fin2011 = llll2011 +lll2011+ll2011+l2011)

trimestre2011<-data.frame(union2011[,c(14:17)])
write.csv(trimestre2011, file = "trim2011.csv")

ano2011<-data.frame(union2011[,c(18)])
write.csv(ano2011, file = "ano2011.csv")





rm(list=ls())
#2012
#ene
ene2012<- read.csv("enero2012.csv")
ene2012<- data.frame(ene2012[,c(2:3)])
names(ene2012)<- c("DPTO","suma201201")
#feb
feb2012<- read.csv("Febrero2012.csv")
feb2012<- data.frame(feb2012[,c(3)])
names(feb2012)<- c("suma201202")

#mar
mar2012<- read.csv("Marzo2012.csv")
mar2012<- data.frame(mar2012[,c(3)])
names(mar2012)<- c("suma201203")

#abr
abr2012<- read.csv("Abril2012.csv")
abr2012<- data.frame(abr2012[,c(3)])
names(abr2012)<- c("suma201204")

#may
may2012<- read.csv("Mayo2012.csv")
may2012<- data.frame(may2012[,c(3)])
names(may2012)<- c("suma201205")

#jun
jun2012<- read.csv("Junio2012.csv")
jun2012<- data.frame(jun2012[,c(3)])
names(jun2012)<- c("suma201206")

#jul
jul2012<- read.csv("Julio2012.csv")
jul2012<- data.frame(jul2012[,c(3)])
names(jul2012)<- c("suma201207")

#ago
ago2012<- read.csv("Agosto2012.csv")
ago2012<- data.frame(ago2012[,c(3)])
names(ago2012)<- c("suma201208")

#sep
sep2012<- read.csv("Septiembre2012.csv")
sep2012<- data.frame(sep2012[,c(3)])
names(sep2012)<- c("suma201209")

#oct
oct2012<- read.csv("Octubre2012.csv")
oct2012<- data.frame(oct2012[,c(3)])
names(oct2012)<- c("suma201210")

#nov
nov2012<- read.csv("Noviembre2012.csv")
nov2012<- data.frame(nov2012[,c(3)])
names(nov2012)<- c("suma201211")

#dic
dic2012<- read.csv("Diciembre2012.csv")
dic2012<- data.frame(dic2012[,c(3)])
names(dic2012)<- c("suma201212")


#Sumatoria por trimestres y años
union2012<-cbind(ene2012,feb2012,mar2012,abr2012,may2012,jun2012,jul2012,ago2012,sep2012,oct2012,nov2012,dic2012)
union2012 <-union2012 %>% mutate(l2012 = suma201201 + suma201202 + suma201203) %>% 
  mutate(ll2012 = suma201204 + suma201205 + suma201206) %>% 
  mutate(lll2012 = suma201207 + suma201208 + suma201209) %>% 
  mutate(llll2012 = suma201210 + suma201211 + suma201212)%>% 
  mutate(fin2012 = llll2012 +lll2012+ll2012+l2012)

trimestre2012<-data.frame(union2012[,c(14:17)])
write.csv(trimestre2012, file = "trim2012.csv")

ano2012<-data.frame(union2012[,c(18)])
write.csv(ano2012, file = "ano2012.csv")






rm(list=ls())
#2013
#ene
ene2013<- read.csv("enero2013.csv")
ene2013<- data.frame(ene2013[,c(2:3)])
names(ene2013)<- c("DPTO","suma201301")
#feb
feb2013<- read.csv("Febrero2013.csv")
feb2013<- data.frame(feb2013[,c(3)])
names(feb2013)<- c("suma201302")

#mar
mar2013<- read.csv("Marzo2013.csv")
mar2013<- data.frame(mar2013[,c(3)])
names(mar2013)<- c("suma201303")

#abr
abr2013<- read.csv("Abril2013.csv")
abr2013<- data.frame(abr2013[,c(3)])
names(abr2013)<- c("suma201304")

#may
may2013<- read.csv("Mayo2013.csv")
may2013<- data.frame(may2013[,c(3)])
names(may2013)<- c("suma201305")

#jun
jun2013<- read.csv("Junio2013.csv")
jun2013<- data.frame(jun2013[,c(3)])
names(jun2013)<- c("suma201306")

#jul
jul2013<- read.csv("Julio2013.csv")
jul2013<- data.frame(jul2013[,c(3)])
names(jul2013)<- c("suma201307")

#ago
ago2013<- read.csv("Agosto2013.csv")
ago2013<- data.frame(ago2013[,c(3)])
names(ago2013)<- c("suma201308")

#sep
sep2013<- read.csv("Septiembre2013.csv")
sep2013<- data.frame(sep2013[,c(3)])
names(sep2013)<- c("suma201309")

#oct
oct2013<- read.csv("Octubre2013.csv")
oct2013<- data.frame(oct2013[,c(3)])
names(oct2013)<- c("suma201310")

#nov
nov2013<- read.csv("Noviembre2013.csv")
nov2013<- data.frame(nov2013[,c(3)])
names(nov2013)<- c("suma201311")

#dic
dic2013<- read.csv("Diciembre2013.csv")
dic2013<- data.frame(dic2013[,c(3)])
names(dic2013)<- c("suma201312")


#Sumatoria por trimestres y años
union2013<-cbind(ene2013,feb2013,mar2013,abr2013,may2013,jun2013,jul2013,ago2013,sep2013,oct2013,nov2013,dic2013)
union2013 <-union2013 %>% mutate(l2013 = suma201301 + suma201302 + suma201303) %>% 
  mutate(ll2013 = suma201304 + suma201305 + suma201306) %>% 
  mutate(lll2013 = suma201307 + suma201308 + suma201309) %>% 
  mutate(llll2013 = suma201310 + suma201311 + suma201312)%>% 
  mutate(fin2013 = llll2013 +lll2013+ll2013+l2013)

trimestre2013<-data.frame(union2013[,c(14:17)])
write.csv(trimestre2013, file = "trim2013.csv")

ano2013<-data.frame(union2013[,c(18)])
write.csv(ano2013, file = "ano2013.csv")






rm(list=ls())
#2014
#ene
ene2014<- read.csv("enero2014.csv")
ene2014<- data.frame(ene2014[,c(2:3)])
names(ene2014)<- c("DPTO","suma201401")
#feb
feb2014<- read.csv("Febrero2014.csv")
feb2014<- data.frame(feb2014[,c(3)])
names(feb2014)<- c("suma201402")

#mar
mar2014<- read.csv("Marzo2014.csv")
mar2014<- data.frame(mar2014[,c(3)])
names(mar2014)<- c("suma201403")

#abr
abr2014<- read.csv("Abril2014.csv")
abr2014<- data.frame(abr2014[,c(3)])
names(abr2014)<- c("suma201404")

#may
may2014<- read.csv("Mayo2014.csv")
may2014<- data.frame(may2014[,c(3)])
names(may2014)<- c("suma201405")

#jun
jun2014<- read.csv("Junio2014.csv")
jun2014<- data.frame(jun2014[,c(3)])
names(jun2014)<- c("suma201406")

#jul
jul2014<- read.csv("Julio2014.csv")
jul2014<- data.frame(jul2014[,c(3)])
names(jul2014)<- c("suma201407")

#ago
ago2014<- read.csv("Agosto2014.csv")
ago2014<- data.frame(ago2014[,c(3)])
names(ago2014)<- c("suma201408")

#sep
sep2014<- read.csv("Septiembre2014.csv")
sep2014<- data.frame(sep2014[,c(3)])
names(sep2014)<- c("suma201409")

#oct
oct2014<- read.csv("Octubre2014.csv")
oct2014<- data.frame(oct2014[,c(3)])
names(oct2014)<- c("suma201410")

#nov
nov2014<- read.csv("Noviembre2014.csv")
nov2014<- data.frame(nov2014[,c(3)])
names(nov2014)<- c("suma201411")

#dic
dic2014<- read.csv("Diciembre2014.csv")
dic2014<- data.frame(dic2014[,c(3)])
names(dic2014)<- c("suma201412")


#Sumatoria por trimestres y años
union2014<-cbind(ene2014,feb2014,mar2014,abr2014,may2014,jun2014,jul2014,ago2014,sep2014,oct2014,nov2014,dic2014)
union2014 <-union2014 %>% mutate(l2014 = suma201401 + suma201402 + suma201403) %>% 
  mutate(ll2014 = suma201404 + suma201405 + suma201406) %>% 
  mutate(lll2014 = suma201407 + suma201408 + suma201409) %>% 
  mutate(llll2014 = suma201410 + suma201411 + suma201412)%>% 
  mutate(fin2014 = llll2014 +lll2014+ll2014+l2014)

trimestre2014<-data.frame(union2014[,c(14:17)])
write.csv(trimestre2014, file = "trim2014.csv")

ano2014<-data.frame(union2014[,c(18)])
write.csv(ano2014, file = "ano2014.csv")




rm(list=ls())
#2015
#ene
ene2015<- read.csv("enero2015.csv")
ene2015<- data.frame(ene2015[,c(2:3)])
names(ene2015)<- c("DPTO","suma201501")
#feb
feb2015<- read.csv("Febrero2015.csv")
feb2015<- data.frame(feb2015[,c(3)])
names(feb2015)<- c("suma201502")

#mar
mar2015<- read.csv("Marzo2015.csv")
mar2015<- data.frame(mar2015[,c(3)])
names(mar2015)<- c("suma201503")

#abr
abr2015<- read.csv("Abril2015.csv")
abr2015<- data.frame(abr2015[,c(3)])
names(abr2015)<- c("suma201504")

#may
may2015<- read.csv("Mayo2015.csv")
may2015<- data.frame(may2015[,c(3)])
names(may2015)<- c("suma201505")

#jun
jun2015<- read.csv("Junio2015.csv")
jun2015<- data.frame(jun2015[,c(3)])
names(jun2015)<- c("suma201506")

#jul
jul2015<- read.csv("Julio2015.csv")
jul2015<- data.frame(jul2015[,c(3)])
names(jul2015)<- c("suma201507")

#ago
ago2015<- read.csv("Agosto2015.csv")
ago2015<- data.frame(ago2015[,c(3)])
names(ago2015)<- c("suma201508")

#sep
sep2015<- read.csv("Septiembre2015.csv")
sep2015<- data.frame(sep2015[,c(3)])
names(sep2015)<- c("suma201509")

#oct
oct2015<- read.csv("Octubre2015.csv")
oct2015<- data.frame(oct2015[,c(3)])
names(oct2015)<- c("suma201510")

#nov
nov2015<- read.csv("Noviembre2015.csv")
nov2015<- data.frame(nov2015[,c(3)])
names(nov2015)<- c("suma201511")

#dic
dic2015<- read.csv("Diciembre2015.csv")
dic2015<- data.frame(dic2015[,c(3)])
names(dic2015)<- c("suma201512")


#Sumatoria por trimestres y años
union2015<-cbind(ene2015,feb2015,mar2015,abr2015,may2015,jun2015,jul2015,ago2015,sep2015,oct2015,nov2015,dic2015)
union2015 <-union2015 %>% mutate(l2015 = suma201501 + suma201502 + suma201503) %>% 
  mutate(ll2015 = suma201504 + suma201505 + suma201506) %>% 
  mutate(lll2015 = suma201507 + suma201508 + suma201509) %>% 
  mutate(llll2015 = suma201510 + suma201511 + suma201512)%>% 
  mutate(fin2015 = llll2015 +lll2015+ll2015+l2015)

trimestre2015<-data.frame(union2015[,c(14:17)])
write.csv(trimestre2015, file = "trim2015.csv")

ano2015<-data.frame(union2015[,c(18)])
write.csv(ano2015, file = "ano2015.csv")



rm(list=ls())
#2016
#ene
ene2016<- read.csv("enero2016.csv")
ene2016<- data.frame(ene2016[,c(2:3)])
names(ene2016)<- c("DPTO","suma201601")
#feb
feb2016<- read.csv("Febrero2016.csv")
feb2016<- data.frame(feb2016[,c(3)])
names(feb2016)<- c("suma201602")

#mar
mar2016<- read.csv("Marzo2016.csv")
mar2016<- data.frame(mar2016[,c(3)])
names(mar2016)<- c("suma201603")

#abr
abr2016<- read.csv("Abril2016.csv")
abr2016<- data.frame(abr2016[,c(3)])
names(abr2016)<- c("suma201604")

#may
may2016<- read.csv("Mayo2016.csv")
may2016<- data.frame(may2016[,c(3)])
names(may2016)<- c("suma201605")

#jun
jun2016<- read.csv("Junio2016.csv")
jun2016<- data.frame(jun2016[,c(3)])
names(jun2016)<- c("suma201606")

#jul
jul2016<- read.csv("Julio2016.csv")
jul2016<- data.frame(jul2016[,c(3)])
names(jul2016)<- c("suma201607")

#ago
ago2016<- read.csv("Agosto2016.csv")
ago2016<- data.frame(ago2016[,c(3)])
names(ago2016)<- c("suma201608")

#sep
sep2016<- read.csv("Septiembre2016.csv")
sep2016<- data.frame(sep2016[,c(3)])
names(sep2016)<- c("suma201609")

#oct
oct2016<- read.csv("Octubre2016.csv")
oct2016<- data.frame(oct2016[,c(3)])
names(oct2016)<- c("suma201610")

#nov
nov2016<- read.csv("Noviembre2016.csv")
nov2016<- data.frame(nov2016[,c(3)])
names(nov2016)<- c("suma201611")

#dic
dic2016<- read.csv("Diciembre2016.csv")
dic2016<- data.frame(dic2016[,c(3)])
names(dic2016)<- c("suma201612")


#Sumatoria por trimestres y años
union2016<-cbind(ene2016,feb2016,mar2016,abr2016,may2016,jun2016,jul2016,ago2016,sep2016,oct2016,nov2016,dic2016)
union2016 <-union2016 %>% mutate(l2016 = suma201601 + suma201602 + suma201603) %>% 
  mutate(ll2016 = suma201604 + suma201605 + suma201606) %>% 
  mutate(lll2016 = suma201607 + suma201608 + suma201609) %>% 
  mutate(llll2016 = suma201610 + suma201611 + suma201612)%>% 
  mutate(fin2016 = llll2016 +lll2016+ll2016+l2016)

trimestre2016<-data.frame(union2016[,c(14:17)])
write.csv(trimestre2016, file = "trim2016.csv")

ano2016<-data.frame(union2016[,c(18)])
write.csv(ano2016, file = "ano2016.csv")



rm(list=ls())
#2017
#ene
ene2017<- read.csv("enero2017.csv")
ene2017<- data.frame(ene2017[,c(2:3)])
names(ene2017)<- c("DPTO","suma201701")
#feb
feb2017<- read.csv("Febrero2017.csv")
feb2017<- data.frame(feb2017[,c(3)])
names(feb2017)<- c("suma201702")

#mar
mar2017<- read.csv("Marzo2017.csv")
mar2017<- data.frame(mar2017[,c(3)])
names(mar2017)<- c("suma201703")

#abr
abr2017<- read.csv("Abril2017.csv")
abr2017<- data.frame(abr2017[,c(3)])
names(abr2017)<- c("suma201704")

#may
may2017<- read.csv("Mayo2017.csv")
may2017<- data.frame(may2017[,c(3)])
names(may2017)<- c("suma201705")

#jun
jun2017<- read.csv("Junio2017.csv")
jun2017<- data.frame(jun2017[,c(3)])
names(jun2017)<- c("suma201706")

#jul
jul2017<- read.csv("Julio2017.csv")
jul2017<- data.frame(jul2017[,c(3)])
names(jul2017)<- c("suma201707")

#ago
ago2017<- read.csv("Agosto2017.csv")
ago2017<- data.frame(ago2017[,c(3)])
names(ago2017)<- c("suma201708")

#sep
sep2017<- read.csv("Septiembre2017.csv")
sep2017<- data.frame(sep2017[,c(3)])
names(sep2017)<- c("suma201709")

#oct
oct2017<- read.csv("Octubre2017.csv")
oct2017<- data.frame(oct2017[,c(3)])
names(oct2017)<- c("suma201710")

#nov
nov2017<- read.csv("Noviembre2017.csv")
nov2017<- data.frame(nov2017[,c(3)])
names(nov2017)<- c("suma201711")

#dic
dic2017<- read.csv("Diciembre2017.csv")
dic2017<- data.frame(dic2017[,c(3)])
names(dic2017)<- c("suma201712")


#Sumatoria por trimestres y años
union2017<-cbind(ene2017,feb2017,mar2017,abr2017,may2017,jun2017,jul2017,ago2017,sep2017,oct2017,nov2017,dic2017)
union2017 <-union2017 %>% mutate(l2017 = suma201701 + suma201702 + suma201703) %>% 
  mutate(ll2017 = suma201704 + suma201705 + suma201706) %>% 
  mutate(lll2017 = suma201707 + suma201708 + suma201709) %>% 
  mutate(llll2017 = suma201710 + suma201711 + suma201712)%>% 
  mutate(fin2017 = llll2017 +lll2017+ll2017+l2017)

trimestre2017<-data.frame(union2017[,c(14:17)])
write.csv(trimestre2017, file = "trim2017.csv")

ano2017<-data.frame(union2017[,c(18)])
write.csv(ano2017, file = "ano2017.csv")




rm(list=ls())
#2018
#ene
ene2018<- read.csv("enero2018.csv")
ene2018<- data.frame(ene2018[,c(2:3)])
names(ene2018)<- c("DPTO","suma201801")
#feb
feb2018<- read.csv("Febrero2018.csv")
feb2018<- data.frame(feb2018[,c(3)])
names(feb2018)<- c("suma201802")

#mar
mar2018<- read.csv("Marzo2018.csv")
mar2018<- data.frame(mar2018[,c(3)])
names(mar2018)<- c("suma201803")

#abr
abr2018<- read.csv("Abril2018.csv")
abr2018<- data.frame(abr2018[,c(3)])
names(abr2018)<- c("suma201804")

#may
may2018<- read.csv("Mayo2018.csv")
may2018<- data.frame(may2018[,c(3)])
names(may2018)<- c("suma201805")

#jun
jun2018<- read.csv("Junio2018.csv")
jun2018<- data.frame(jun2018[,c(3)])
names(jun2018)<- c("suma201806")

#jul
jul2018<- read.csv("Julio2018.csv")
jul2018<- data.frame(jul2018[,c(3)])
names(jul2018)<- c("suma201807")

#ago
ago2018<- read.csv("Agosto2018.csv")
ago2018<- data.frame(ago2018[,c(3)])
names(ago2018)<- c("suma201808")

#sep
sep2018<- read.csv("Septiembre2018.csv")
sep2018<- data.frame(sep2018[,c(3)])
names(sep2018)<- c("suma201809")

#oct
oct2018<- read.csv("Octubre2018.csv")
oct2018<- data.frame(oct2018[,c(3)])
names(oct2018)<- c("suma201810")

#nov
nov2018<- read.csv("Noviembre2018.csv")
nov2018<- data.frame(nov2018[,c(3)])
names(nov2018)<- c("suma201811")

#dic
dic2018<- read.csv("Diciembre2018.csv")
dic2018<- data.frame(dic2018[,c(3)])
names(dic2018)<- c("suma201812")


#Sumatoria por trimestres y años
union2018<-cbind(ene2018,feb2018,mar2018,abr2018,may2018,jun2018,jul2018,ago2018,sep2018,oct2018,nov2018,dic2018)
union2018 <-union2018 %>% mutate(l2018 = suma201801 + suma201802 + suma201803) %>% 
  mutate(ll2018 = suma201804 + suma201805 + suma201806) %>% 
  mutate(lll2018 = suma201807 + suma201808 + suma201809) %>% 
  mutate(llll2018 = suma201810 + suma201811 + suma201812)%>% 
  mutate(fin2018 = llll2018 +lll2018+ll2018+l2018)

trimestre2018<-data.frame(union2018[,c(14:17)])
write.csv(trimestre2018, file = "trim2018.csv")

ano2018<-data.frame(union2018[,c(18)])
write.csv(ano2018, file = "ano2018.csv")





rm(list=ls())
#2019
#ene
ene2019<- read.csv("enero2019.csv")
ene2019<- data.frame(ene2019[,c(2:3)])
names(ene2019)<- c("DPTO","suma201901")
#feb
feb2019<- read.csv("Febrero2019.csv")
feb2019<- data.frame(feb2019[,c(3)])
names(feb2019)<- c("suma201902")

#mar
mar2019<- read.csv("Marzo2019.csv")
mar2019<- data.frame(mar2019[,c(3)])
names(mar2019)<- c("suma201903")

#abr
abr2019<- read.csv("Abril2019.csv")
abr2019<- data.frame(abr2019[,c(3)])
names(abr2019)<- c("suma201904")

#may
may2019<- read.csv("Mayo2019.csv")
may2019<- data.frame(may2019[,c(3)])
names(may2019)<- c("suma201905")

#jun
jun2019<- read.csv("Junio2019.csv")
jun2019<- data.frame(jun2019[,c(3)])
names(jun2019)<- c("suma201906")

#jul
jul2019<- read.csv("Julio2019.csv")
jul2019<- data.frame(jul2019[,c(3)])
names(jul2019)<- c("suma201907")

#ago
ago2019<- read.csv("Agosto2019.csv")
ago2019<- data.frame(ago2019[,c(3)])
names(ago2019)<- c("suma201908")

#sep
sep2019<- read.csv("Septiembre2019.csv")
sep2019<- data.frame(sep2019[,c(3)])
names(sep2019)<- c("suma201909")

#oct
oct2019<- read.csv("Octubre2019.csv")
oct2019<- data.frame(oct2019[,c(3)])
names(oct2019)<- c("suma201910")

#nov
nov2019<- read.csv("Noviembre2019.csv")
nov2019<- data.frame(nov2019[,c(3)])
names(nov2019)<- c("suma201911")

#dic
dic2019<- read.csv("Diciembre2019.csv")
dic2019<- data.frame(dic2019[,c(3)])
names(dic2019)<- c("suma201912")


#Sumatoria por trimestres y años
union2019<-cbind(ene2019,feb2019,mar2019,abr2019,may2019,jun2019,jul2019,ago2019,sep2019,oct2019,nov2019,dic2019)
union2019 <-union2019 %>% mutate(l2019 = suma201901 + suma201902 + suma201903) %>% 
  mutate(ll2019 = suma201904 + suma201905 + suma201906) %>% 
  mutate(lll2019 = suma201907 + suma201908 + suma201909) %>% 
  mutate(llll2019 = suma201910 + suma201911 + suma201912)%>% 
  mutate(fin2019 = llll2019 +lll2019+ll2019+l2019)

trimestre2019<-data.frame(union2019[,c(14:17)])
write.csv(trimestre2019, file = "trim2019.csv")

ano2019<-data.frame(union2019[,c(18)])
write.csv(ano2019, file = "ano2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
rm(list=ls())
#Trimestres y años
trim2009<- read.csv("trim2009.csv")
trim2010<- read.csv("trim2010.csv")
trim2011<- read.csv("trim2011.csv")
trim2012<- read.csv("trim2012.csv")
trim2013<- read.csv("trim2013.csv")
trim2014<- read.csv("trim2014.csv")
trim2015<- read.csv("trim2015.csv")
trim2016<- read.csv("trim2016.csv")
trim2017<- read.csv("trim2017.csv")
trim2018<- read.csv("trim2018.csv")
trim2019<- read.csv("trim2019.csv")

ano2009<- read.csv("ano2009.csv")
ano2010<- read.csv("ano2010.csv")
ano2011<- read.csv("ano2011.csv")
ano2012<- read.csv("ano2012.csv")
ano2013<- read.csv("ano2013.csv")
ano2014<- read.csv("ano2014.csv")
ano2015<- read.csv("ano2015.csv")
ano2016<- read.csv("ano2016.csv")
ano2017<- read.csv("ano2017.csv")
ano2018<- read.csv("ano2018.csv")
ano2019<- read.csv("ano2019.csv")





trimestres<-cbind(trim2009,trim2010,trim2011,trim2012,trim2013,trim2014,trim2015,trim2016,trim2017,trim2018,trim2019)
anos<-cbind(ano2009,ano2010,ano2011,ano2012,ano2013,ano2014,ano2015,ano2016,ano2017,ano2018,ano2019)
write.csv(trimestres, file = "trimestres.csv")
write.csv(anos, file = "anos.csv")


