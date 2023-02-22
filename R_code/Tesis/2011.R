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
setwd("Documents/Tesis/Bases de datos/")

#NUNCA UTILIZAR FORMATO csv O SI SE UTILIZA NO TRANSFORMARLAS EN VARIABLES NUMERICAS YA QUE CAMBIA SU VALOR

############################################################################
#                                      2011                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2011            #
rm(list=ls())

#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201101 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "enero2011.csv")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Febrero  2011            #

#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/탍ea - Ocupados.sav")


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
laboral3 <-laboral2 %>% mutate(suma201102 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Febrero2011.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Marzo 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/탍ea - Ocupados.sav")


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
laboral3 <-laboral2 %>% mutate(suma201103 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Marzo2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Abril 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201104 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Abril2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Mayo 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201105 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Mayo2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Junio 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201106 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Junio2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Julio 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201107 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Julio2011.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Agosto 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201108 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Agosto2011.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Septiembre 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201109 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Septiembre2011.csv")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Octubre 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201110 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Octubre2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Noviembre 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/탍ea - Ocupados.sav")

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
laboral3 <-laboral2 %>% mutate(suma201111 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Noviembre2011.csv")



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

rm(list=ls())

#                     Diciembre 2011           #
#Archivos
Cabecera<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/탍ea - Ocupados.sav")



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
laboral3 <-laboral2 %>% mutate(suma201112 = ing_lab +ing_lab1 + ing_lab2) 
laboral3<- data.frame(laboral3[,c(1,5)])
write.csv(laboral3, file = "Diciembre2011.csv")


