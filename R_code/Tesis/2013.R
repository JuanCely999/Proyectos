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

#str(separate(data =  Cabecera,  col  =  Fex_c_2011,   into =  c("urbana", " total", "urbana2", " total2", " total3"), sep  =  ","))

#Cabecera2 <- Cabecera %>% separate(`Cabecera...c.178..`, into = c("urbana", " total3"), sep = ". ")
#select(na.omit(concat.split.multiple(melt(Cabecera, id.vars="Cabecera...c.178.."), split.col="Cabecera...c.178..", sep=".", direction="long")), -time)
#Cabecera$Fex_c_2011= as.factor(Cabecera$Fex_c_2011)
#`Cabecera...Ocupados.(1)`<- data.frame(`Cabecera...Ocupados.(1)`[,c(175:178)])
#trim091<-cbind(enero09, Febrero09,Marzo09)
#trim0912 <-trim091 %>% mutate(sumatrim = suma) 
#names (trim091) = c("1", "2", "3", "4", "5", "6", "7")
setwd("Documents/Tesis/Bases de datos/Ingresos laboral")

#NUNCA UTILIZAR FORMATO csv O SI SE UTILIZA NO TRANSFORMARLAS EN VARIABLES NUMERICAS YA QUE CAMBIA SU VALOR

############################################################################
#                                      2013                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2013            #
rm(list=ls())

#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Enero/Enero/탍ea - Ocupados.sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201301 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2013.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2013            #

#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Febrero/Febrero/탍ea - Ocupados.sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201302 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2013.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Marzo/Marzo/탍ea - Ocupados.sav")


#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201303 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Abril/Abril/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201304 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Mayo/Mayo/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201305 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Junio/Junio/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201306 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Julio/Julio/탍ea - Ocupados.sav")


#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201307 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2013.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Agosto/Agosto/탍ea - Ocupados.sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201308 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2013.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/탍ea - Ocupados.sav")



#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201309 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2013.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Octubre/Octubre/탍ea - Ocupados.sav")



#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201310 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/탍ea - Ocupados.sav")



#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201311 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2013.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2013           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/탍ea - Ocupados.sav")

#Cabecera
Cabecera[is.na(Cabecera)] <- 0
cabecera2<- Cabecera %>% mutate(in_lab = Inglabo * Fex_c_2011)
cab <- cabecera2 %>% group_by(Dpto) %>% summarise(ing_lab = sum(in_lab))

#Resto
resto[is.na(resto)] <- 0
resto3<- resto %>% mutate(in_lab1 = Inglabo * Fex_c_2011) 
res <- resto3 %>% group_by(Dpto) %>% summarise(ing_lab1 = sum(in_lab1))

laboral <- cab %>% 
  left_join(res)

#Area
area[is.na(area)] <- 0
area3<- area %>% mutate(in_lab2 = Inglabo * Fex_c_2011) 
are <- area3 %>% group_by(Dpto) %>% summarise(ing_lab2 = sum(in_lab2))

#union
laboral2 <- laboral %>% 
  left_join(are)

laboral2[is.na(laboral2)] <- 0
laboral3 <-laboral2 %>% mutate(suma201312 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2013.csv")


