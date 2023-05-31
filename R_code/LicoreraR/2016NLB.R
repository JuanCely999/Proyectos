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
#                                      2016                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2016            #
#2016
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/탍ea - Vivienda y Hogares.sav")

#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(resto2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("01" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("01" , b)
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
ano<-rep("2016" , b)
mes<-rep("01" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,53,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
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


a20161<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161")])


#                     Febrero  2016            #

#feb
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/탍ea - Ocupados.sav")

#feb
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("02" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("02" , b)
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
ano<-rep("2016" , b)
mes<-rep("02" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,53,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
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


a20162<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162")])





#                     Marzo  2016            #

#mar
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/탍ea - Ocupados.sav")

#marz
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("03" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("03" , b)
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
ano<-rep("2016" , b)
mes<-rep("03" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,53,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
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


a20163<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163")])





#                     abril  2016            #

#abr
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/탍ea - Ocupados.sav")

#abr
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("04" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
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
ano<-rep("2016" , b)
mes<-rep("04" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,53,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,53,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,53,59)]) 
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


a20164<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164")])





#                     mayo  2016            #

#may
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/탍ea - Ocupados.sav")

#may
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/탍ea - Vivienda y Hogares.sav")

names(cab_viv)
#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("05" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
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
ano<-rep("2016" , b)
mes<-rep("05" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)
names(cab_viv)
#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a20165<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164","a20165")])





#                     junio  2016            #

#jun
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/탍ea - Ocupados.sav")

#jun
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("06" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("06" , b)
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
ano<-rep("2016" , b)
mes<-rep("06" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a20166<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164","a20165","a20166")])








#                     julio  2016            #

#juli
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/탍ea - Ocupados.sav")

#juli
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("07" , b)
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
ano<-rep("2016" , b)
mes<-rep("07" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a20167<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164","a20165","a20166","a20167")])






#                     Agosto  2016            #

#agos
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/탍ea - Ocupados.sav")

#agos
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("08" , b)
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
ano<-rep("2016" , b)
mes<-rep("08" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a20168<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164","a20165","a20166","a20167","a20168")])







#                     septiembre  2016            #

#sep
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/탍ea - Ocupados.sav")

#sep
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("09" , b)
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
ano<-rep("2016" , b)
mes<-rep("09" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a20169<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164",
  "a20165","a20166","a20167","a20168", "a20169")])





#                     octubre 2016            #

#oct
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/탍ea - Ocupados.sav")

#oct
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("10" , b)
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
ano<-rep("2016" , b)
mes<-rep("10" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a201610<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164",
                           "a20165","a20166","a20167","a20168", "a20169",
                           "a201610")])




#                     noviembre 2016            #

#nov
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/탍ea - Ocupados.sav")

#nov
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("11" , b)
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
ano<-rep("2016" , b)
mes<-rep("11" , b)
area4<- data.frame(area3,ano,mes,name)
##NO SE ENCUENTRA

pob <- rbind(cab4, resto4)


#cab_viv
cab_viv1<- data.frame(cab_viv[,c(1,49,59)]) 
names(cab_viv1) 
names(cab_viv1)= c("id", "estrato","Dpto")
cab_viv2<-filter(cab_viv1, Dpto==15 )
names(cab_viv2)
names(cab_viv2)= c("id", "estrato","dpto2")
cab_viv3<-cab_viv2[!duplicated(cab_viv2$id), ]
names(cab_viv2)
cab_viv4<- na.omit(cab_viv3)

#resto_viv
resto_viv1<- data.frame(resto_viv[,c(1,49,59)]) 
names(resto_viv1)  
names(resto_viv1)= c("id", "estrato","Dpto")
resto_viv2<-filter(resto_viv1, Dpto==15 )
names(resto_viv2)
names(resto_viv2)= c("id", "estrato","dpto2")
resto_viv3<-resto_viv2[!duplicated(resto_viv2$id), ]
names(resto_viv3)
resto_viv4<- na.omit(resto_viv3)

#cab_viv
area_viv1<- data.frame(area_viv[,c(1,49,59)]) 
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


a201611<-pob2 %>% right_join(pob, by="id")


#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164",
                           "a20165","a20166","a20167","a20168", "a20169",
                           "a201610", "a201611")])






#                     diciembre 2016            #

#dic
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/탍ea - Ocupados.sav")

#dic
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/탍ea - Vivienda y Hogares.sav")

names(cab_viv)
#####Cab
cab1<- data.frame(cab[,c(1,151,153:154)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")  
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2016" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,151,153:154)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2016" , b)
mes<-rep("12" , b)
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
ano<-rep("2016" , b)
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


a201612<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","a20161","a20162","a20163","a20164",
                           "a20165","a20166","a20167","a20168", "a20169",
                           "a201610", "a201611","a201612")])



final2016 <- rbind(a20161,a20162,a20163,a20164,
                           a20165,a20166,a20167,a20168, a20169,
                           a201610, a201611,a201612)




rm(list=ls()[! ls() %in% c("final2015","final2016")])
