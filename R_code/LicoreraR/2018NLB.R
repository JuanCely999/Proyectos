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
#                                      2018                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2018            #
#2018
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/Cabecera.csv")
resto<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/resto.csv")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/area.csv")

cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/area_viv.csv")
names(cab)
#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(resto2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("01" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("01" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("01" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20181<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181")])


#                     Febrero  2018            #

#feb
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/area.csv")

#feb
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("02" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("02" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("02" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20182<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182")])





#                     Marzo  2018            #

#mar
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/area.csv")

#marz
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("03" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("03" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("03" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20183<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183")])





#                     abril  2018            #

#abr
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/area.csv")

#abr
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("04" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("04" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("04" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20184<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184")])





#                     mayo  2018            #

#may
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/area.csv")

#may
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("05" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("05" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("05" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20185<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184","a20185")])





#                     junio  2018            #

#jun
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/area.csv")

#jun
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("06" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("06" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("06" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20186<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184","a20185","a20186")])








#                     julio  2018            #

#juli
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/탍ea - Ocupados.dta")

#juli
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/탍ea - Vivienda y Hogares.dta")

names(cab_viv)
#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("07" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("07" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,58)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,58)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,58)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20187<-pob2 %>% right_join(pob, by="id")

#a201711$estrato<-fct_recode(a201711$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")


#a20187$estrato<-fct_recode(a20187$estrato, "1"="1", "2"="2", "3"="3","4"="4","5"="5")

table(a20187$estrato)

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184","a20185","a20186","a20187")])






#                     Agosto  2018            #

#agos
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/탍ea - Ocupados.dta")

#agos
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("08" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("08" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,58)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,58)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,58)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20188<-pob2 %>% right_join(pob, by="id")

table(a20188$estrato)

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184","a20185","a20186","a20187","a20188")])







#                     septiembre  2018            #

#sep
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/탍ea - Ocupados.dta")

#sep
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("09" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("09" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,58)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,58)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,58)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a20189<-pob2 %>% right_join(pob, by="id")


table(a20189$estrato)
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184",
  "a20185","a20186","a20187","a20188", "a20189")])





#                     octubre 2018            #

#oct
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/area.csv")

#oct
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("10" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("10" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,2,3)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,2,3)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,2,3)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a201810<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184",
                           "a20185","a20186","a20187","a20188", "a20189",
                           "a201810")])




#                     noviembre 2018            #

#nov
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/Resto - Ocupados.dta")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/탍ea - Ocupados.csv")

#nov
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/탍ea - Vivienda y Hogares.dta")

#names(cab_viv)
#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("11" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("11" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,58)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,58)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,58)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a201811<-pob2 %>% right_join(pob, by="id")

#write.csv(a201811, file = "a201811.csv")
#table(a201811$estrato)
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184",
                           "a20185","a20186","a20187","a20188", "a20189",
                           "a201810", "a201811")])






#                     diciembre 2018            #

#dic
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/탍ea - Ocupados.dta")

#dic
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164,165)]) 
names(cab1)  
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2018" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164,165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2018" , b)
mes<-rep("12" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164,165)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2018" , b)
mes<-rep("12" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,58)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,58)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,58)]) 
names(area_viv1) 
names(area_viv1)= c("id", "estrato","Dpto")
area_viv2<-filter(area_viv1, Dpto==15 )
names(area_viv2)
names(area_viv2)= c("id", "estrato","dpto2")
area_viv3<-area_viv2[!duplicated(area_viv2$id), ]
names(area_viv3)
area_viv4<- na.omit(area_viv3)
#NO SE ENCUENTRA

pob2 <- rbind(cab_viv4, resto_viv4)


a201812<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("a20181","a20182","a20183","a20184",
                           "a20185","a20186","a20187","a20188", "a20189",
                           "a201810", "a201811","a201812")])



final2018 <- rbind(a20181,a20182,a20183,a20184,
                           a20185,a20186,a20187,a20188, a20189,
                           a201810, a201811,a201812)




rm(list=ls()[! ls() %in% c("final2018")])

#save_2018 <- rbind(final2015,final2016,final2017,final2018)
#write.csv(final2015, file = "save_2018.csv")



