##############################################################################
###Desagregacion bases de datos ingresos laborales desde GEIH##
#http://microdatos.dane.gov.co/index.php/catalog/207/get_microdata
######################Autor: Juan Pablo Cely#################################
###############################15-03-2023####################################
############################################################################
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)
#                                      2020                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2020            #
#2020
cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,150,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
 #                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("01" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20201<-cab_viv4 %>% right_join(cab4, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201")])


#                     Febrero  2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,150,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("02" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20202<-cab_viv4 %>% right_join(cab4, by="id")

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202")])





#                     Marzo  2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Marzo/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

#cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#names(cab)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,12,11,168,11,10,7)])  
names(cab1)   
names(cab1)= c("id","estrato","dpto2", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "estrato","dpto2","ingreso","dpto","exp","name")
table(cab3$estrato)
cab3$estrato <- factor(cab3$name, labels = c("marzo"))
table(cab3$estrato)
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("03" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
#
#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20203<-cab4

#################################################

#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203")])





#                     abril  2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Abril/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

#cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
names(cab)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,11,10,151,10,9,6)])  
names(cab1)   
names(cab1)= c("id","estrato","dpto2", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
sum(is.na(cab2$ingreso))
names(cab3)= c("id", "estrato","dpto2","ingreso","dpto","exp","name")
table(cab3$estrato)
cab3$estrato <- factor(cab3$name, labels = c("abril"))
table(cab3$estrato)
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("04" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
#
#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20204<-cab4


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204")])





#                     mayo  2020            #


cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Mayo/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

#cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
names(cab)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,12,11,152,11,10,6)])  
names(cab1)   
names(cab1)= c("id","estrato","dpto2", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
sum(is.na(cab2$ingreso))
names(cab3)= c("id", "estrato","dpto2","ingreso","dpto","exp","name")
table(cab3$estrato)
cab3$estrato <- factor(cab3$name, labels = c("mayo"))
table(cab3$estrato)
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("05" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
#
#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20205<-cab4


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204","a20205")])





#                     junio  2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Junio/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

#cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
names(cab)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,12,11,152,11,10,6)])  
names(cab1)   
names(cab1)= c("id","estrato","dpto2", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
sum(is.na(cab2$ingreso))
names(cab3)= c("id", "estrato","dpto2","ingreso","dpto","exp","name")
table(cab3$estrato)
cab3$estrato <- factor(cab3$name, labels = c("junio"))
table(cab3$estrato)
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("06" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
#
#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20206<-cab4


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204","a20205","a20206")])








#                     julio  2020            #

#juli
cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Julio/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

#cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Febrero/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
names(cab)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,12,11,152,11,10,6)])  
names(cab1)   
names(cab1)= c("id","estrato","dpto2", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
sum(is.na(cab2$ingreso))
names(cab3)= c("id", "estrato","dpto2","ingreso","dpto","exp","name")
table(cab3$estrato)
cab3$estrato <- factor(cab3$name, labels = c("julio"))
table(cab3$estrato)
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
#
#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20207<-cab4



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204","a20205","a20206","a20207")])






#                     Agosto  2020            #


cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Agosto/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Agosto/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,151,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20208<-cab_viv4 %>% right_join(cab4, by="id")

###
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204","a20205","a20206","a20207","a20208")])







#                     septiembre  2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Septiembre/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Septiembre/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,151,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a20209<-cab_viv4 %>% right_join(cab4, by="id")
################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204",
  "a20205","a20206","a20207","a20208", "a20209")])





#                     octubre 2020            #
cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Octubre/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Octubre/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,151,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a202010<-cab_viv4 %>% right_join(cab4, by="id")
##############

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204",
                           "a20205","a20206","a20207","a20208", "a20209",
                           "a202010")])




#                     noviembre 2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Noviembre/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Noviembre/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,151,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a202011<-cab_viv4 %>% right_join(cab4, by="id")
##############

#################
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204",
                           "a20205","a20206","a20207","a20208", "a20209",
                           "a202010", "a202011")])






#                     diciembre 2020            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Diciembre/Ocupados.DTA")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Diciembre/Vivienda y Hogares.DTA")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
cab1<- data.frame(cab[,c(1,151,11,10,6)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp","name")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp","name")
a<-dim(cab3)
b<-a[1]
#cab$name<- ifelse(edad > 60, "anciano", edad)
table(cab3$name)
cab3$name <- factor(cab3$name, labels = c("cab", "resto"))
table(cab3$name)
#cab3$name <- as.numeric(cab3$name,
#                   labels = c("1", "2"))
#name <- rep("cab" , b)
ano<-rep("2020" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes)



# 
# #####resto
# 
# resto1<- data.frame(resto[,c(1,151,153:154)])  
# names(resto1) 
# names(resto1)= c("id", "ingreso","Dpto","exp")
# resto2<-filter(resto1, Dpto==15 )
# names(resto2)
# resto3 <- na.omit(resto2)
# names(resto3)= c("id", "ingreso","dpto","exp")
# a<-dim(resto3)
# b<-a[1]
# name <- rep("resto" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# resto4<- data.frame(resto3,ano,mes, name)
# 
# 
# 
# 
# #####area
# 
# area1<- data.frame(area[,c(1,151,153:154)]) 
# names(area1)  
# names(area1)= c("id", "ingreso","Dpto","exp")
# area2<-filter(area1, Dpto==15 )
# names(area2)
# area3 <- na.omit(area2)
# names(area3)= c("id", "ingreso","dpto","exp")
# a<-dim(area3)
# b<-a[1]
# name <- rep("area" , b)
# ano<-rep("2020" , b)
# mes<-rep("01" , b)
# area4<- data.frame(area3,ano,mes,name)
# ##NO SE ENCUENTRA

#pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,55,8)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
# resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
# names(resto_viv1)  
# names(resto_viv1)= c("id", "estrato","Dpto")
# resto_viv2<-filter(resto_viv1, Dpto==15 )
# names(resto_viv2)
# names(resto_viv2)= c("id", "estrato","dpto2")
# resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
# names(resto_viv3)
# resto_viv4<- na.omit(resto_viv3)
# 
# #cab_viv
# area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
# names(area_viv1) 
# names(area_viv1)= c("id", "estrato","Dpto")
# area_viv2<-filter(area_viv1, Dpto==15 )
# names(area_viv2)
# names(area_viv2)= c("id", "estrato","dpto2")
# area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
# names(area_viv3)
# area_viv4<- na.omit(area_viv3)
# #NO SE ENCUENTRA

#pob2 <- rbind(cab_viv4, resto_viv4)


a202012<-cab_viv4 %>% right_join(cab4, by="id")
##############


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","a20201","a20202","a20203","a20204",
                           "a20205","a20206","a20207","a20208", "a20209",
                           "a202010", "a202011","a202012")])



final2020 <- rbind(a20201,a20202,a20203,a20204,
                           a20205,a20206,a20207,a20208, a20209,
                           a202010, a202011,a202012)



rm(list=ls()[! ls() %in% c("final2018","final2019","final2020")])

#total5 <- rbind(final2018,final2019,final2020)

#write.csv(total5, file = "total5.csv")
