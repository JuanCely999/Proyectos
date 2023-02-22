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
#                                      2012                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2012            #
rm(list=ls())

#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Enero/01/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Enero/01/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Enero/01/탍ea - Ocupados.sav")


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
laboral3 <-laboral2 %>% mutate(suma201201 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2012.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2012            #

#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Febrero/02/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Febrero/02/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Febrero/02/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201202 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2012.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Marzo/03/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Marzo/03/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Marzo/03/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201203 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Abril/04/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Abril/04/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Abril/04/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201204 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Mayo/05/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Mayo/05/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Mayo/05/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201205 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Junio/06/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Junio/06/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Junio/06/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201206 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Julio/07/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Julio/07/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Julio/07/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201207 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2012.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Agosto/08/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Agosto/08/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Agosto/08/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201208 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2012.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Septiembre/09/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Septiembre/09/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Septiembre/09/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201209 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2012.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Octubre/10/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Octubre/10/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Octubre/10/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201210 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Noviembre/11/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Noviembre/11/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Noviembre/11/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201211 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2012.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2012           #
#Archivos
Cabecera <- read_sav("Ingresos laboral/2012/Diciembre/12/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Diciembre/12/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Diciembre/12/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201212 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2012.csv")


