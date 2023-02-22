##############################################################################
###Estudio estadistico OPREC###########################################
######################Autor: Juan Pablo Cely#################################
######################19/02/2020#################################


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
#############################Barbacoa y tumaco######################
##############################################################

setwd("~/Documents/Investigacion/OPREC/")


esc <- read_excel("Cruces_solos/_51_ipse_oprec_escenario2__202105061310.xlsx")
#paz <- read_excel("Cruces_solos/Usuarios Llorente VF.xlsx")
paz <- read_excel("Cruces_solos/Usuarios Proyecto Barbacoas-Tumaco VF.xlsx")

##############################################################
#############################GENERAL######################
##############################################################

#dup_IA<-view(esc[duplicated(esc$LATITUD), ])
#dup_IA2<- data.frame(dup_IA[,c(1,235,245)])

esc2<-esc %>%
  group_by(LATITUD) %>%
  slice(1)

esc100<- data.frame(esc2[,c(1,5,231)])

##############################################################
#############################################################
paz_id<-view(paz[duplicated(paz$LATITUD), ])
write.csv(paz_id, file = "otros_cruces_dup.csv")

paz2<-paz %>%
  group_by(LATITUD) %>%
  slice(1)

paz_join<-esc2 %>% semi_join(paz2, by="LATITUD")

paz_join2<-esc100 %>% right_join(paz2, by="LATITUD")

write.csv(paz_join, file = "otros_cruces_ori.csv")


#write.csv(paz_join2, file = "otros_cruces_guia_ori.csv")




#############Comprobar#############
compr <- read_excel("Cruces_solos/Barbacoas_tumaco.xlsx")
dup_compr<-view(compr[duplicated(compr$Num_formulario), ])



##############################################################
#############################Llorente#########################
##############################################################
cat("\f")
rm(list = ls())

esc <- read_excel("Cruces_solos/_51_ipse_oprec_escenario2__202105061310.xlsx")
paz <- read_excel("Cruces_solos/Usuarios Llorente VF.xlsx")


esc2<-esc %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

#Verificar que los que no se unieron en IK despues se unan a IA
esc3<-esc %>%
  group_by(Numero_identificacion) %>%
  slice(1)


#write.csv(dup_Ik2, file = "dup_ik.csv")
##############################################################
#############################################################
paz_id<-view(paz[duplicated(paz$`N°IDENTIFICACIÓN`), ])
write.csv(paz_id, file = "llorente_dup_id.csv")

paz2<-paz %>%
  group_by(`N°IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`N°IDENTIFICACIÓN`))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")
write.csv(paz_nojoinIA, file = "llorente_no join.csv")


final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))

write.csv(final1, file = "llorente_orig.csv")


##############################################################
#############################Tumaco (Gran Rosario)#############
##############################################################
rm(list=ls()[! ls() %in% c("esc2","esc3")])

paz <- read_excel("Cruces_solos/Usuarios Tumaco (Gran Rosario).xlsx")
paz_id<-view(paz[duplicated(paz$`N°IDENTIFICACIÓN`), ])
write.csv(paz_id, file = "granrosario_dup_id.csv")

paz2<-paz %>%
  group_by(`N°IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`N°IDENTIFICACIÓN`))

paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")
write.csv(paz_nojoinIA, file = "granrosario_no join.csv")


final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))

write.csv(final1, file = "granrosario_orig.csv")



##############################################################
#############################Cumbal#############
##############################################################
rm(list=ls()[! ls() %in% c("esc2","esc3")])
paz <- read_excel("Cruces_solos/Usuarios Cumbal.xlsx")
paz_id<-view(paz[duplicated(paz$IDENTIFICACION), ])
write.csv(paz_id, file = "cumbal_dup_id.csv")

paz2<-paz %>%
  group_by(IDENTIFICACION) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=IDENTIFICACION))

paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")
write.csv(paz_nojoinIA, file = "cumbal_no_join.csv")


final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))

write.csv(final1, file = "cumbal_orig.csv")






#############################################################
#############################Ejercicio extra con Gran rosario#############
##############################################################
rm(list=ls()[! ls() %in% c("esc2","esc3")])
paz <- read_excel(paste("Cruces_solos/Depurar faltantes/Tumaco (Gran Rosario).xlsx",sep=""),2)

paz_id<-view(paz[duplicated(paz$`N°IDENTIFICACIÓN`), ])
#write.csv(paz_id, file = "cumbal_dup_id.csv")


esc4<-esc3 %>%
  group_by(LATITUD) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")


paz_join<-esc4 %>% semi_join(paz, by="LATITUD")

write.csv(paz_join, file = "Tumaco(Rosario)_adicion.csv")










###################################################
###################################################
###################################################
###################################################
cat("\f")
rm(list = ls())
total <- read_excel("Cruces_solos/_51_ipse_oprec_escenario2__202105061310.xlsx")

barba<- filter(total, U_codigo_municipio==52079)
tumac<- filter(total, U_codigo_municipio==52835)

esc<-rbind(barba,tumac)

inst<- filter(esc, Edificacion=="InstituciÃ³n")
viv<- filter(esc, Edificacion=="Vivienda")


a1 <- read_excel("Cruces_solos/Depurar faltantes/LIST COLEGIOS Y CSALUD BARBACOAS (2).xlsx")
a2 <- read_excel("Cruces_solos/Depurar faltantes/INSTITUCIONES TUMACO 2MARZ.xlsx")
a3 <- read_excel("Cruces_solos/Depurar faltantes/Base de datos Barbacoas_tumaco.xlsx")
a4 <- read_excel("Cruces_solos/Depurar faltantes/Listado de usuarios afrotumacos (2).xlsx")
a5 <- read_excel("Cruces_solos/Depurar faltantes/Base de datos tumaco individuales 1.xlsx")
a6 <- read_excel("Cruces_solos/Depurar faltantes/Listado de usuarios Barbacoas 2 20052021.xlsx")
a7 <- read_excel("Cruces_solos/Depurar faltantes/Listado de usuarios barbacoas 1 06042021.xlsx")
a8 <- read_excel("Cruces_solos/Depurar faltantes/Tumaco (Gran Rosario).xlsx")
a9 <- read_excel("Cruces_solos/Depurar faltantes/Llorente.xlsx")

a11<- data.frame(a1[,c(1)])
names(a11)<- c("id")

a22<- data.frame(a2[,c(1)])
names(a22)<- c("id")

a33<- data.frame(a3[,c(1)])
names(a33)<- c("id")

a44<- data.frame(a4[,c(2)])
names(a44)<- c("id")

a55<- data.frame(a5[,c(1)])
names(a55)<- c("id")

a66<- data.frame(a6[,c(2)])
names(a66)<- c("id")

a77<- data.frame(a7[,c(2)])
names(a77)<- c("id")

a88<- data.frame(a8[,c(1)])
names(a88)<- c("id")

a99<- data.frame(a9[,c(1)])
names(a99)<- c("id")

list_ins<- rbind(a11,a22)
list_final<- rbind(a33,a44,a55,a66,a77,a88,a99)



paz_id<-view(list_final[duplicated(list_final$id), ])
#write.csv(paz_id, file = "otros_cruces_dup.csv")

paz2<-list_final %>%
  group_by(id) %>%
  slice(1)

esc2<-esc %>%
 group_by(Num_formulario) %>%
  slice(1)

#viv2<-viv%>%
 # group_by(Num_formulario) %>%
#  slice(1)

gen_r<-rename(esc2, c(id=Num_formulario))
#gen_r4<-rename(viv2, c(id=Num_formulario))


#paz_nojoin<-gen_r %>% anti_join(paz2, by="id")

uni<-gen_r %>% semi_join(paz2, by="id")
table(uni$Edificacion)

paz_nojoin2<-gen_r %>% anti_join(paz2, by="id")

table(paz_nojoin2$Edificacion)

dup2<-view(paz_nojoin2[duplicated(paz_nojoin2$Numero_identificacion_consentimiento), ])
write.csv(dup2, file = "vivienda_dup.csv")

dup_correc<-paz_nojoin2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

write.csv(dup_correc, file = "vivienda_orig.csv")


##############################################################################
##############################################################################
rm(list=ls()[! ls() %in% c("list_ins","inst")])


paz2<-list_ins %>%
  group_by(id) %>%
  slice(1)

#esc2<-esc %>%
# group_by(Numero_identificacion_consentimiento) %>%
#slice(1)

inst2<-inst%>%
  group_by(Num_formulario) %>%
  slice(1)


#gen_r<-rename(esc2, c(id=Num_formulario))
gen_r4<-rename(inst2, c(id=Num_formulario))


#paz_nojoin<-gen_r %>% anti_join(paz2, by="id")

paz_nojoin2<-gen_r4 %>% anti_join(paz2, by="id")

dup2<-view(paz_nojoin2[duplicated(paz_nojoin2$Numero_identificacion_consentimiento), ])
write.csv(dup2, file = "instituciones_dup.csv")

dup_correc<-paz_nojoin2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

write.csv(dup_correc, file = "instituciones_orig.csv")


