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
#                                      2019                                #
############################################################################
#--------------------------------------------------------------------------#
#                     Enero  2019            #
#2019
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/탍ea - Ocupados.dta")

cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/탍ea - Vivienda y Hogares.dta")
#names(cab)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20191<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191")])


#                     Febrero  2019            #

#feb
cab<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/Cabecera.csv")
resto<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/resto.csv")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/area.csv")
#feb
cab_viv<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Febrero/area_viv.csv")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20192<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192")])





#                     Marzo  2019            #

#mar
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/탍ea - Ocupados.dta")

#marz
cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20193<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193")])





#                     abril  2019            #

#abr
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/탍ea - Ocupados.dta")

#abr
cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20194<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194")])





#                     mayo  2019            #

#may
cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/탍ea - Ocupados.dta")

#may
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20195<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194","a20195")])





#                     junio  2019            #

#jun
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/area.csv")

#jun
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Junio/area_viv.csv")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20196<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194","a20195","a20196")])








#                     julio  2019            #

#juli
cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Cabecera - Ocupados.dta")
resto<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Resto - Ocupados.dta")
area<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/탍ea - Ocupados.dta")

#juli
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20197<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194","a20195","a20196","a20197")])






#                     Agosto  2019            #

#agos
cab<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/탍ea - Ocupados.dta")

#agos
cab_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read.dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20198<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194","a20195","a20196","a20197","a20198")])







#                     septiembre  2019            #

#sep
cab<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/탍ea - Ocupados.dta")

#sep
cab_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a20199<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194",
  "a20195","a20196","a20197","a20198", "a20199")])





#                     octubre 2019            #

cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/area.csv")

#oct
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Octubre/area_viv.csv")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a201910<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194",
                           "a20195","a20196","a20197","a20198", "a20199",
                           "a201910")])




#                     noviembre 2019            #

#nov
cab<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/Cabecera.csv")
resto<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/resto.csv")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/area.csv")

#nov
cab_viv<-read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Noviembre/area_viv.csv")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a201911<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194",
                           "a20195","a20196","a20197","a20198", "a20199",
                           "a201910", "a201911")])






#                     diciembre 2019            #

#dic
cab<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Cabecera - Ocupados.dta")
resto<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Resto - Ocupados.dta")
area<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/탍ea - Ocupados.dta")

#dic
cab_viv<-read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/탍ea - Vivienda y Hogares.dta")


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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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
ano<-rep("2019" , b)
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


a201912<-pob2 %>% right_join(pob, by="id")



#################################################
#################################################
#################################################
rm(list=ls()[! ls() %in% c("final2018","a20191","a20192","a20193","a20194",
                           "a20195","a20196","a20197","a20198", "a20199",
                           "a201910", "a201911","a201912")])



final2019 <- rbind(a20191,a20192,a20193,a20194,
                           a20195,a20196,a20197,a20198, a20199,
                           a201910, a201911,a201912)




rm(list=ls()[! ls() %in% c("final2018","final2019")])

