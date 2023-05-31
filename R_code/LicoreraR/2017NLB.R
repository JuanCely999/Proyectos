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
#                                      2017                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2017            #
#2017
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/탍ea - Ocupados.sav")

cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/탍ea - Vivienda y Hogares.sav")
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("01" , b)
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


a20171<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171")])


#                     Febrero  2017            #

#feb
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/탍ea - Ocupados.sav")

#feb
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/탍ea - Vivienda y Hogares.sav")


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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("02" , b)
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


a20172<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172")])





#                     Marzo  2017            #

#mar
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/탍ea - Ocupados.sav")

#marz
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/탍ea - Vivienda y Hogares.sav")


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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("03" , b)
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


a20173<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173")])





#                     abril  2017            #

#abr
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/탍ea - Ocupados.sav")

#abr
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/탍ea - Vivienda y Hogares.sav")


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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("04" , b)
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


a20174<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174")])





#                     mayo  2017            #

#may
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/탍ea - Ocupados.sav")

#may
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/탍ea - Vivienda y Hogares.sav")


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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("05" , b)
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


a20175<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174","a20175")])





#                     junio  2017            #

#jun
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/탍ea - Ocupados.sav")

#jun
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/탍ea - Vivienda y Hogares.sav")


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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
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
ano<-rep("2017" , b)
mes<-rep("06" , b)
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


a20176<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174","a20175","a20176")])








#                     julio  2017            #

#juli
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/탍ea - Ocupados.sav")

#juli
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("07" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,164:165)])  
names(resto1)= c("id", "ingreso","Dpto","exp")
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("07" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
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


a20177<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174","a20175","a20176","a20177")])






#                     Agosto  2017            #

#agos
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/탍ea - Ocupados.sav")

#agos
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/탍ea - Vivienda y Hogares.sav")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("08" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,164:165)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp") 
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("08" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
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


a20178<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174","a20175","a20176","a20177","a20178")])







#                     septiembre  2017            #

#sep
#cab<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/Cabecera - Ocupados.dta")
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/area.csv")

#sep
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/cabecera_viv.csv")
resto_viv <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/resto_viv.csv")
area_viv <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("09" , b)
cab4<- data.frame(cab3,ano,mes,name)


names(resto_viv)

#####resto

resto1<- data.frame(resto[,c(1,162,164:165)]) 
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp") 
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("09" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
mes<-rep("09" , b)
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


a20179<-pob2 %>% right_join(pob, by="id")

a20179$estrato<-fct_recode(a20179$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

table(a20179$estrato)
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174",
  "a20175","a20176","a20177","a20178", "a20179")])





#                     octubre 2017            #

#oct
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/area.csv")

#oct
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp") 
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("10" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,164:165)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp") 
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("10" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
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


a201710<-pob2 %>% right_join(pob, by="id")

a201710$estrato<-fct_recode(a201710$estrato, "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

table(a201710$estrato)
sum(is.na(a201710$estrato))
#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174",
                           "a20175","a20176","a20177","a20178", "a20179",
                           "a201710")])




#                     noviembre 2017            #

#nov
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/area.csv")

#nov
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")   
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("11" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,164:165)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp") 
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("11" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
mes<-rep("11" , b)
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


a201711<-pob2 %>% right_join(pob, by="id")

a201711$estrato<-fct_recode(a201711$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

table(a201711$estrato)

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174",
                           "a20175","a20176","a20177","a20178", "a20179",
                           "a201710", "a201711")])






#                     diciembre 2017            #

#dic
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/area.csv")

#dic
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/area_viv.csv")


#####Cab
cab1<- data.frame(cab[,c(1,162,164:165)])  
names(cab1)   
names(cab1)= c("id", "ingreso","Dpto","exp")   
cab2<-filter(cab1, Dpto==15 )
names(cab2)
cab3 <- na.omit(cab2)
names(cab3)= c("id", "ingreso","dpto","exp")
a<-dim(cab3)
b<-a[1]
name <- rep("cab" , b)
ano<-rep("2017" , b)
mes<-rep("12" , b)
cab4<- data.frame(cab3,ano,mes,name)




#####resto

resto1<- data.frame(resto[,c(1,162,164:165)])  
names(resto1) 
names(resto1)= c("id", "ingreso","Dpto","exp") 
resto2<-filter(resto1, Dpto==15 )
names(resto2)
resto3 <- na.omit(resto2)
names(resto3)= c("id", "ingreso","dpto","exp")
a<-dim(resto3)
b<-a[1]
name <- rep("resto" , b)
ano<-rep("2017" , b)
mes<-rep("12" , b)
resto4<- data.frame(resto3,ano,mes, name)




#####area

area1<- data.frame(area[,c(1,162,164:165)]) 
names(area1)  
names(area1)= c("id", "ingreso","Dpto","exp")
area2<-filter(area1, Dpto==15 )
names(area2)
area3 <- na.omit(area2)
names(area3)= c("id", "ingreso","dpto","exp")
a<-dim(area3)
b<-a[1]
name <- rep("area" , b)
ano<-rep("2017" , b)
mes<-rep("12" , b)
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


a201712<-pob2 %>% right_join(pob, by="id")

a201712$estrato<-fct_recode(a201712$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

table(a2012$estrato)

#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2015","final2016","a20171","a20172","a20173","a20174",
                           "a20175","a20176","a20177","a20178", "a20179",
                           "a201710", "a201711","a201712")])



final2017 <- rbind(a20171,a20172,a20173,a20174,
                           a20175,a20176,a20177,a20178, a20179,
                           a201710, a201711,a201712)

#recuperacion2017<-rbind(a20179,a201710, a201711,a201712)

#write.csv(recuperacion2017, file = "recuperacion2017.csv")
# 
# 
# rm(list=ls()[! ls() %in% c("final2015","final2016","final2017")])


total1 <- rbind(final2015,final2016,final2017)

#write.csv(total1, file = "total1.csv")


