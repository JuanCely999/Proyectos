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
#                                      2019                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2019            #
rm(list=ls())

#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201901 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2019.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2019            #

#Archivos
Cabecera <-read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201902 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2019.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201903 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201904 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201905 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2019           #
#Archivos
Cabecera <-read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201906 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201907 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2019.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201908 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2019.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201909 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2019.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2019           #
#Archivos
Cabecera <-read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201910 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2019           #
#Archivos
Cabecera <-read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/area.csv")

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
laboral3 <-laboral2 %>% mutate(suma201911 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2019.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2019           #
#Archivos
Cabecera <-read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/탍ea - Ocupados.dta")

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
laboral3 <-laboral2 %>% mutate(suma201912 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2019.csv")


