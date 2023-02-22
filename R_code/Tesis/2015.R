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
#                                      2015                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2015            #
rm(list=ls())

#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Enero/Enero/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201501 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2015.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2015            #

#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Febrero/Febrero/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201502 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2015.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Marzo/Marzo/탍ea - Ocupados.sav")


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
laboral3 <-laboral2 %>% mutate(suma201503 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Abril/Abril/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201504 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Mayo/Mayo/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201505 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Junio/Junio/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201506 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Julio/Julio/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201507 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2015.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Agosto/Agosto/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201508 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2015.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201509 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2015.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Octubre/Octubre/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201510 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201511 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2015.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2015           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201512 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2015.csv")


