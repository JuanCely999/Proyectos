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
#                                      2021                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2021            #
#2021
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/탍ea - Ocupados.dta")

cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/탍ea - Vivienda y Hogares.dta")
names(cab_viv)
#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(resto2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("01" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164:165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("01" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164:165)]) 
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("01" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20211<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211")])


#                     Febrero  2021            #

#feb
#feb
cab<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/area.csv")
#feb
cab_viv<-read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Febrero/area_viv.csv")
names(cab)

#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("02" , b)
cab4<- data.frame(cab3,ano,mes,name)

names(resto)


#####resto

resto1<- data.frame(resto[,c(1,162,163:164)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("02" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("02" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20212<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212")])





#                     Marzo  2021            #

#mar
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/탍ea - Ocupados.dta")

#marz
cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("03" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,163:164)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("03" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164:165)]) 
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("03" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)])
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20213<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213")])





#                     abril  2021            #

#abr
cab<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/Cabecera - Ocupados.csv")
resto<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/Resto - Ocupados.csv")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/area.csv")
#feb
cab_viv<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/Cabecera - Vivienda y Hogares.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/Resto - Vivienda y Hogares.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("04" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,163:164)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("04" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,151,153:154)])
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("04" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20214<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214")])





#                     mayo  2021            #

cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/탍ea - Ocupados.dta")

#may
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)  
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("05" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,163:164)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("05" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,151,153:154)])
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("05" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)])
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)])
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


a20215<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214","a20215")])





#                     junio  2021            #

cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/탍ea - Ocupados.dta")

#may
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Junio/탍ea - Vivienda y Hogares.dta")

#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("06" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,163:164)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("06" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("06" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)])
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)])
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


a20216<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214","a20215","a20216")])








#                     julio  2021            #

#juli
cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/탍ea - Ocupados.dta")

#may
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,163,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,163:164)])  
names(resto1)
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("07" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,163,164:165)]) 
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("07" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1)
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)])
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


a20217<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214","a20215","a20216","a20217")])






#                     Agosto  2021            #

cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/Cabecera - Ocupados.dta")
resto <- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/Resto - Ocupados.dta")
area <- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/area.csv")

#jun
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/area_viv.csv")

names(cab)
#####Cab
cab1<- data.frame(cab[,c(1,162,163:164)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto
names(resto)
resto1<- data.frame(resto[,c(1,161,162:163)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("08" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,163:164)]) 
names(area1) 
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("08" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)])
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)])
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20218<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214","a20215","a20216","a20217","a20218")])







#                     septiembre  2021            #

cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/탍ea - Ocupados.dta")

#may
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/탍ea - Vivienda y Hogares.dta")


#####Cab
cab1<- data.frame(cab[,c(1,162,163:164)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,161,162:163)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("09" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,163:164)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("09" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a20219<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214",
  "a20215","a20216","a20217","a20218", "a20219")])





#                     octubre 2021            #

#oct

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/Cabecera - Ocupados.DTA")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/Resto - Ocupados.DTA")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/탍ea - Ocupados.DTA")

#may
cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/Cabecera - Vivienda y Hogares.DTA")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/Resto - Vivienda y Hogares.DTA")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Octubre/탍ea - Vivienda y Hogares.DTA")

names(resto)
#####Cab
cab1<- data.frame(cab[,c(1,164,165,166)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,163,164:165)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("10" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,163:164)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("10" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a202110<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214",
                           "a20215","a20216","a20217","a20218", "a20219",
                           "a202110")])




#                     noviembre 2021            #

#nov
cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/Cabecera - Ocupados.DTA")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/Resto - Ocupados.DTA")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/탍ea - Ocupados.DTA")

#may
cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/Cabecera - Vivienda y Hogares.DTA")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/Resto - Vivienda y Hogares.DTA")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Noviembre/탍ea - Vivienda y Hogares.DTA")


#####Cab
cab1<- data.frame(cab[,c(1,162,163:164)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto
names(resto)
resto1<- data.frame(resto[,c(1,161,162:163)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("11" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,163:164)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("11" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)]) 
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)]) 
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


a202111<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214",
                           "a20215","a20216","a20217","a20218", "a20219",
                           "a202110", "a202111")])






#                     diciembre 2021            #

cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/Cabecera - Ocupados.DTA")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/Resto - Ocupados.DTA")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/탍ea - Ocupados.DTA")

#may
cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/Cabecera - Vivienda y Hogares.DTA")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/Resto - Vivienda y Hogares.DTA")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/탍ea - Vivienda y Hogares.DTA")


#####Cab
cab1<- data.frame(cab[,c(1,162,163:164)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2021" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,161,162:163)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2021" , b)
mes<-rep("12" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,163:164)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2021" , b)
mes<-rep("12" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,48,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,48,59)])
names(resto_viv1) 
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,48,59)])
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


a202112<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","a20211","a20212","a20213","a20214",
                           "a20215","a20216","a20217","a20218", "a20219",
                           "a202110", "a202111","a202112")])



final2021 <- rbind(a20211,a20212,a20213,a20214,
                           a20215,a20216,a20217,a20218, a20219,
                           a202110, a202111,a202112)




rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021")])
