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
#                                      2022                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2022            #
#2022
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Enero/Ocupados.csv")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Enero/Datos del hogar y la vivienda.csv")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(1,160,157,159,148)])  
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
ano<-rep("2022" , b)
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

names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,34,43)]) 
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


a20221<-cab_viv4 %>% right_join(cab4, by="id")


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221")])


#                     Febrero  2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Febrero/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Febrero/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(1,155,158,160,148)])  
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
ano<-rep("2022" , b)
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

names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,34,43)]) 
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


a20222<-cab_viv4 %>% right_join(cab4, by="id")


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222")])





#                     Marzo  2022            #


cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Marzo/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Marzo/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(1,155,158,160,148)])  
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
ano<-rep("2022" , b)
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

names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,34,43)]) 
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


a20223<-cab_viv4 %>% right_join(cab4, by="id")

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223")])





#                     abril  2022            #


cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Abril/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Abril/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
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

names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
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


a20224<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224")])





#                     mayo  2022            #


cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Mayo/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Mayo/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
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

names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
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


a20225<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224","a20225")])





#                     junio  2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Junio/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Junio/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("06" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a20226<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
#########################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224","a20225","a20226")])








#                     julio  2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Julio/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Julio/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a20227<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224","a20225","a20226","a20227")])






#                     Agosto  2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Agosto/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Agosto/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a20228<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224","a20225","a20226","a20227","a20228")])







#                     septiembre  2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Septiembre/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Septiembre/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a20229<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224",
  "a20225","a20226","a20227","a20228", "a20229")])





#                     octubre 2022            #
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Octubre/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Octubre/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a202210<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224",
                           "a20225","a20226","a20227","a20228", "a20229",
                           "a202210")])




#                     noviembre 2022            #
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Noviembre/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Noviembre/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a202211<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################
##############################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224",
                           "a20225","a20226","a20227","a20228", "a20229",
                           "a202210", "a202211")])






#                     diciembre 2022            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Diciembre/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2022/Diciembre/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2022" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a202212<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224",
                           "a20225","a20226","a20227","a20228", "a20229",
                           "a202210", "a202211","a202212")])




##################################################
##################################################
##################################################



#                     enero 2023            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2023/Enero/Ocupados.CSV", sep=";")
# resto<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Ocupados.sav")
# area<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2023/Enero/Datos del hogar y la vivienda.CSV", sep=";")
# resto_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/Resto - Vivienda y Hogares.sav")
# area_viv<- read_sav("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2020/Enero/Enero/탍ea - Vivienda y Hogares.sav")
#https://www.youtube.com/watch?v=4DPwdAkzhuc
#table(cab$CLASE)
#EMM2<- filter(EMM, Real== "Producción real")
#####Cab
names(cab)
cab1<- data.frame(cab[,c(4,197,12,11,10)])  
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
ano<-rep("2023" , b)
mes<-rep("01" , b)
cab4<- data.frame(cab3,ano,mes)



names(cab_viv)
#cab_viv
cab_viv1<- data.frame(cab_viv[,c(4,16,11)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)


a20231<-cab_viv4 %>% right_join(cab4, by="id")
#################################################
##############################################








#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","a20221","a20222","a20223","a20224",
                           "a20225","a20226","a20227","a20228", "a20229",
                           "a202210", "a202211","a202212","a20231")])



final2022 <- rbind(a20221,a20222,a20223,a20224,
                           a20225,a20226,a20227,a20228, a20229,
                           a202210, a202211,a202212,a20231)




rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","final2022")])

#total3 <- rbind(final2021,final2022)

#write.csv(total3, file = "total3.csv")




