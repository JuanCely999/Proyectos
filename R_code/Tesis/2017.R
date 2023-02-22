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
#                                      2017                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2017            #
rm(list=ls())

#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Enero/Enero/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201701 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2017.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2017            #

#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Febrero/Febrero/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201702 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2017.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Marzo/Marzo/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201703 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Abril/Abril/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201704 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Mayo/Mayo/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201705 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Junio/Junio/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201706 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Julio/Julio/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201707 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2017.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2017           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2017/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Agosto/Agosto/햞ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201708 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2017.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2017           #
#Archivos
Cabecera <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/area.csv")


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
laboral3 <-laboral2 %>% mutate(suma201709 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2017.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2017           #
#Archivos
Cabecera <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/area.csv")


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
laboral3 <-laboral2 %>% mutate(suma201710 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2017           #
#Archivos
Cabecera <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201711 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2017.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2017           #
#Archivos
Cabecera <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201712 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2017.csv")


