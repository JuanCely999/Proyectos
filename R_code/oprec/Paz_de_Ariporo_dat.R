##############################################################################
###Estudio estadistico OPREC###########################################
######################Autor: Juan Pablo Cely#################################
######################16/02/2020#################################

library(foreign)
library(readxl)
library(ggplot2)  
library(readxl)
library(tidyr)
library(dplyr)
library(haven)
library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)
library(reshape2)

library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
library(RColorBrewer)
library(fmsb)
cat("\f")
rm(list = ls())


##############################################################
#############################Paz de Ariporo######################
##############################################################


setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),3)


##############################################################
#############################GENERAL######################
##############################################################
names(general)
esc<- filter(general, U_codigo_municipio==85250)

dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
dup_IA2<- data.frame(dup_IA[,c(1,235,245)])

dup_Ik<-view(esc[duplicated(esc$Numero_identificacion_consentimiento), ])
dup_Ik2<- data.frame(dup_Ik[,c(1,235,245)])


esc2<-esc %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

#Verificar que los que no se unieron en IK despues se unan a IA
esc3<-esc %>%
  group_by(Numero_identificacion) %>%
  slice(1)


#write.csv(dup_Ik2, file = "dup_ik.csv")

##############################################################
#############################Paz del Rio######################
#############################################################
paz_id<-view(paz[duplicated(paz$`NUMERO IDENTIFICACIÓN`), ])
#write.csv(paz_id, file = "paz_id.csv")
paz2<-paz %>%
  group_by(`NUMERO IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

##############################################################
#############################Comunidades indigenas#############
#############################################################
com_id<-view(com[duplicated(com$`NUMERO IDENTIFICACIÓN`), ])
#write.csv(com_id, file = "com_id.csv")

com2<-com %>%
  group_by(`NUMERO IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

##############################################################
#############################Otros#############
#############################################################
otro_id<-view(otro[duplicated(otro$`NUMERO IDENTIFICACIÓN`), ])
#write.csv(otro_id, file = "otro_id.csv")

otro2<-otro %>%
  group_by(`NUMERO IDENTIFICACIÓN`) %>%
  slice(1)



##############################################################
#############################Tratamiento de datos#############
#############################################################
gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`NUMERO IDENTIFICACIÓN`))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")
#write.csv(paz_nojoin, file = "paz_nojoin.csv")
class(paz_nojoin$id)
class(no_gen$id)

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")

#paz_nojoinIA<- data.frame(paz_nojoinIA[,c(1,235,245)])
#write.csv(paz_nojoinIA, file = "paz_nojoinIA.csv")

final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
final2<- rename(paz_nojoinIA, c(Numero_identificacion=id))

final1$Numero_identificacion= as.numeric(final1$Numero_identificacion)


#fin_tot<- rbind(final1,final2)

a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])

a3<- data.frame(final2[,c(1:254)])
a4<- data.frame(final2[,c(255)])

write.csv(a1, file = "Otros1.csv")
write.csv(a2, file = "Otros2.csv")
write.csv(a3, file = "Otros3.csv")
write.csv(a4, file = "Otros4.csv")

#dep<- read_excel(paste("Depuracion comunidades indigenas.xlsx",sep=""),3)


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
  #group_by(id) %>%
  #slice(1)




##############################################################
#############################Ultimo tratamiento#############
#############################################################

cat("\f")
rm(list = ls())

cruzados<- read_excel(paste("Usuarios Que Se Cruzan Con Otros Proyectos Pza (3).xlsx",sep=""),2)
dep1<- read_excel(paste("Depuracion comunidades indigenas.xlsx",sep=""),1)
#dep2<- read_excel(paste("Depuracion comunidades indigenas.xlsx",sep=""),2)
#dep3<- read_excel(paste("Depuracion comunidades indigenas.xlsx",sep=""),3)

#dep_total<- rbind(dep1,dep2,dep3)


cruz<-rename(cruzados, c(id=`TELEFONO CELULAR`))
#dept<-rename(dep_total, c(id=Telefono_celular_encuestado))
dept<-rename(dep1, c(id=Telefono_celular_encuestado))

cruz$id[is.na(cruz$id)] <- 0

cruz_t<-dept %>% anti_join(cruz, by="id")
cruz_t2<-dept %>% semi_join(cruz, by="id")


cruz_fin<-rename(cruz_t, c(Telefono_celular_encuestado=id))
cruz_no<-rename(cruz_t2, c(Telefono_celular_encuestado=id))

a1<- data.frame(cruz_fin[,c(1:254)])
a2<- data.frame(cruz_fin[,c(255:256)])

a3<- data.frame(cruz_no[,c(1:254)])
a4<- data.frame(cruz_no[,c(255:256)])

write.csv(a1, file = "Cruzados1.csv")
write.csv(a2, file = "Cruzados2.csv")
write.csv(a3, file = "Cruzados no1.csv")
write.csv(a4, file = "Cruzados no2.csv")




##########################################################
##########################################################
##########################################################
##########################################################
######################Tumaco#############################
##########################################################
##########################################################
##########################################################
cat("\f")
rm(list = ls())
setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")

tumaco<- filter(general, U_codigo_municipio==52835)

table(tumaco$U_vereda)
table(tumaco$U_corregimiento)

tum11<-filter(tumaco, U_vereda=="LA GUAYACANA")

tum11dup<-tum11[duplicated(tum11$Numero_identificacion_consentimiento), ]
write.csv(tum11dup, file = "Guayacana_dup.csv")

tum111<-tum11 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)




tum12<-filter(tumaco, U_corregimiento=="HONDA")
tum13<-filter(tumaco, U_corregimiento=="PEÑA LISA")

tum_fin_1_2<-rbind(tum12,tum13)

tum_fin_1_2dup<-tum_fin_1_2[duplicated(tum_fin_1_2$Numero_identificacion_consentimiento), ]
write.csv(tum_fin_1_2dup, file = "Honda_Lisa.csv")

tum_fin_1_22<-tum_fin_1_2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)


honda<-filter(tum_fin_1_22, U_corregimiento=="HONDA")
lisa<-filter(tum_fin_1_22, U_corregimiento=="PEÑA LISA")






tum21<-filter(tumaco, U_vereda=="LLORENTE")
tum21_dup<-tum21[duplicated(tum21$Numero_identificacion_consentimiento), ]
write.csv(tum21_dup, file = "Llorente.csv")

tum212<-tum21 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

tum22<-filter(tumaco, U_vereda=="CORREGIMIENTO LLORENTE")

tum22_dup<-tum22[duplicated(tum22$Numero_identificacion_consentimiento), ]
write.csv(tum22_dup, file = "Corre Llorente.csv")

tum222<-tum22 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)


tum_fin_2<-rbind(tum212,tum222)



write.csv(tum111, file = "Tumaco1.csv")
write.csv(tum_fin_1_22, file = "Tumaco12.csv")
write.csv(tum_fin_2, file = "Tumaco2.csv")


##########################################################
##########################################################
##########################################################
##########################################################
######################Barbacoa#############################
##########################################################
##########################################################
##########################################################
cat("\f")
rm(list = ls())
setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")


barbacoa<- filter(general, U_codigo_municipio==52079)

table(barbacoa$U_vereda)


table(barbacoa$U_corregimiento)
#####Corregimientos###############

bar_c1<-filter(barbacoa, U_corregimiento=="CABECERA SONADORA")
bar_c2<-filter(barbacoa, U_corregimiento=="HOJAL LA TURBIA")
bar_c3<-filter(barbacoa, U_corregimiento=="JUANA PASCAL")
bar_c4<-filter(barbacoa, U_corregimiento=="PALIZADA")
bar_c5<-filter(barbacoa, U_corregimiento=="QUEJUAMBI LA LISA")
bar_c6<-filter(barbacoa, U_corregimiento=="SALTO LA TURBIA")
bar_c7<-filter(barbacoa, U_corregimiento=="SALVI")
bar_c8<-filter(barbacoa, U_corregimiento=="SONADORA")
bar_c9<-filter(barbacoa, U_corregimiento=="TURBIA PLANADA")

##########Vereda###############
bar_v1<-filter(barbacoa, U_vereda=="DIVISO")
bar_v2<-filter(barbacoa, U_vereda=="BUENAVISTA")
bar_v3<-filter(barbacoa, U_vereda=="TAJADA")
bar_v4<-filter(barbacoa, U_vereda=="ALTAQUER")
bar_v5<-filter(barbacoa, U_vereda=="JUNIN")
bar_v6<-filter(barbacoa, U_vereda=="PULGANDE")
bar_v7<-filter(barbacoa, U_vereda=="SABALETA")
bar_v8<-filter(barbacoa, U_vereda=="SAN JUAN BAUTISTA")
bar_v9<-filter(barbacoa, U_vereda=="TANGARAL")
bar_v10<-filter(barbacoa, U_vereda=="TELPICITO")
bar_v11<-filter(barbacoa, U_vereda=="TRONQUERIA")
bar_v12<-filter(barbacoa, U_vereda=="LORO")
bar_v13<-filter(barbacoa, U_vereda=="TELEMBI")

dim(bar_c1)
dim(bar_c2)
dim(bar_c3)
dim(bar_c4)
dim(bar_c5)
dim(bar_c6)
dim(bar_c7)
dim(bar_c8)
dim(bar_c9)

dim(bar_v1)
dim(bar_v2)
dim(bar_v3)
dim(bar_v4)
dim(bar_v5)
dim(bar_v6)
dim(bar_v7)
dim(bar_v8)
dim(bar_v9)
dim(bar_v10)
dim(bar_v11)
dim(bar_v12)
dim(bar_v13)

bar_corr2<-rbind(bar_c1, bar_c2, bar_c3, bar_c4, bar_c5, bar_c6, bar_c7, bar_c8, bar_c9)
bar_ver2<-rbind(bar_v1, bar_v2, bar_v3, bar_v4, bar_v5, bar_v6, bar_v7, bar_v8, bar_v9, bar_v10, bar_v11, bar_v12, bar_v13)

dup_bc<-bar_corr2[duplicated(bar_corr2$Numero_identificacion), ]
dup_bv<-bar_ver2[duplicated(bar_ver2$Numero_identificacion), ]

write.csv(dup_bc, file = "Dup_bar2.csv")
write.csv(dup_bv, file = "Dup_bar3.csv")

b_corr_<-bar_corr2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

b_ver<-bar_ver2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

write.csv(b_corr_, file = "final_bar2.csv")
write.csv(b_ver, file = "final_bar3.csv")


#####Corregimientos###############

bar2_c1<-filter(b_corr_, U_corregimiento=="CABECERA SONADORA")
bar2_c2<-filter(b_corr_, U_corregimiento=="HOJAL LA TURBIA")
bar2_c3<-filter(b_corr_, U_corregimiento=="JUANA PASCAL")
bar2_c4<-filter(b_corr_, U_corregimiento=="PALIZADA")
bar2_c5<-filter(b_corr_, U_corregimiento=="QUEJUAMBI LA LISA")
bar2_c6<-filter(b_corr_, U_corregimiento=="SALTO LA TURBIA")
bar2_c7<-filter(b_corr_, U_corregimiento=="SALVI")
bar2_c8<-filter(b_corr_, U_corregimiento=="SONADORA")
bar2_c9<-filter(b_corr_, U_corregimiento=="TURBIA PLANADA")

##########Vereda###############
bar2_v1<-filter(b_ver, U_vereda=="DIVISO")
bar2_v2<-filter(b_ver, U_vereda=="BUENAVISTA")
bar2_v3<-filter(b_ver, U_vereda=="TAJADA")
bar2_v4<-filter(b_ver, U_vereda=="ALTAQUER")
bar2_v5<-filter(b_ver, U_vereda=="JUNIN")
bar2_v6<-filter(b_ver, U_vereda=="PULGANDE")
bar2_v7<-filter(b_ver, U_vereda=="SABALETA")
bar2_v8<-filter(b_ver, U_vereda=="SAN JUAN BAUTISTA")
bar2_v9<-filter(b_ver, U_vereda=="TANGARAL")
bar2_v10<-filter(b_ver, U_vereda=="TELPICITO")
bar2_v11<-filter(b_ver, U_vereda=="TRONQUERIA")
bar2_v12<-filter(b_ver, U_vereda=="LORO")
bar2_v13<-filter(b_ver, U_vereda=="TELEMBI")


dim(bar2_c1)
dim(bar2_c2)
dim(bar2_c3)
dim(bar2_c4)
dim(bar2_c5)
dim(bar2_c6)
dim(bar2_c7)
dim(bar2_c8)
dim(bar2_c9)

dim(bar2_v1)
dim(bar2_v2)
dim(bar2_v3)
dim(bar2_v4)
dim(bar2_v5)
dim(bar2_v6)
dim(bar2_v7)
dim(bar2_v8)
dim(bar2_v9)
dim(bar2_v10)
dim(bar2_v11)
dim(bar2_v12)
dim(bar2_v13)

