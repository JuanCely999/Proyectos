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
#############################Arauca######################
##############################################################

setwd("~/Documents/Investigacion/OPREC/")


general <- read_excel("BASE DE DATOS.xlsx")
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("USUARIOS ARAUCA (11-02-2021).xlsx",sep=""),2)


##############################################################
#############################GENERAL######################
##############################################################
#names(general)
esc<- filter(general, U_codigo_municipio==81001)

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
#############################################################
paz_id<-view(paz[duplicated(paz$IDENTIFICACIÓN), ])
#write.csv(paz_id, file = "paz_id.csv")
paz2<-paz %>%
  group_by(IDENTIFICACIÓN) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=IDENTIFICACIÓN))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")

a1<- data.frame(paz_join[,c(1:254)])
a2<- data.frame(paz_join[,c(255)])

write.csv(a1, file = "Arauca1.csv")
write.csv(a2, file = "Arauca2.csv")


#dep<- read_excel("Original/Arauca.xlsx")


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)

##############################################################
#############################Arauquita######################
##############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("USUARIOS ARAUQUITA (11-02-2021).xlsx",sep=""),2)



##############################################################
#############################GENERAL######################
##############################################################
#names(general)
esc<- filter(general, U_codigo_municipio==81065)

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
#############################################################
paz_id<-view(paz[duplicated(paz$IDENTIFICACIÓN), ])
#write.csv(paz_id, file = "paz_id.csv")
paz2<-paz %>%
  group_by(IDENTIFICACIÓN) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=IDENTIFICACIÓN))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")

paz_join2<- rename(paz_join, c(Numero_identificacion_consentimiento=id))

a1<- data.frame(paz_join2[,c(1:254)])
a2<- data.frame(paz_join2[,c(255)])

write.csv(a1, file = "Arauquita1.csv")
write.csv(a2, file = "Arauquita2.csv")


#dep<- read_excel("Original/Arauquita.xlsx")


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)



##############################################################
#############################PTO Rondon######################
##############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("USUARIOS PTO RONDON (11-02-2021).xlsx",sep=""),2)



##############################################################
#############################GENERAL######################
##############################################################
#names(general)
esc<- filter(general, U_codigo_municipio==81591)

#dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
#dup_IA2<- data.frame(dup_IA[,c(1,235,245)])

#dup_Ik<-view(esc[duplicated(esc$Numero_identificacion_consentimiento), ])
#dup_Ik2<- data.frame(dup_Ik[,c(1,235,245)])


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
paz_id<-view(paz[duplicated(paz$IDENTIFICACIÓN), ])
#write.csv(paz_id, file = "dup_pto.csv")
paz2<-paz %>%
  group_by(IDENTIFICACIÓN) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=IDENTIFICACIÓN))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")
#write.csv(paz_nojoin, file = "paz_nojoin.csv")
class(paz_nojoin$id)
class(no_gen$id)

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")

#paz_nojoinIA<- data.frame(paz_nojoinIA[,c(1,235,245)])
#write.csv(paz_nojoinIA, file = "paz_nojoinIARONDON.csv")

final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
#final2<- rename(paz_nojoinIA, c(Numero_identificacion=id))

final1$Numero_identificacion= as.numeric(final1$Numero_identificacion)



a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])

write.csv(a1, file = "PtoRondon1.csv")
write.csv(a2, file = "PtoRondon2.csv")


#dep<- read_excel("Original/Arauquita.xlsx")


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)



##############################################################
###################COMUNIDAD AFRO TUMACO######################
##############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("LIST COMUNIDAD AFRO TUMACO.xlsx",sep=""),2)



##############################################################
#############################GENERAL######################
##############################################################
#names(general)
esc<- filter(general, U_codigo_municipio==52835)

#dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
#dup_IA2<- data.frame(dup_IA[,c(1,235,245)])

#dup_Ik<-view(esc[duplicated(esc$Numero_identificacion_consentimiento), ])
#dup_Ik2<- data.frame(dup_Ik[,c(1,235,245)])


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
paz_id<-view(paz[duplicated(paz$`N° IDENTIFICACIÓN`), ])
#write.csv(paz_id, file = "dup_afro.csv")
paz2<-paz %>%
  group_by(`N° IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`N° IDENTIFICACIÓN`))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")
#write.csv(paz_nojoin, file = "paz_nojoin.csv")
class(paz_nojoin$id)
class(no_gen$id)

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")

#paz_nojoinIA<- data.frame(paz_nojoinIA[,c(1,235,245)])
#write.csv(paz_nojoinIA, file = "paz_nojoinIARONDON.csv")

final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
final2<- rename(paz_nojoinIA, c(Numero_identificacion=id))

final1$Numero_identificacion= as.numeric(final1$Numero_identificacion)



a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])
a3<- data.frame(final2[,c(1:254)])
a4<- data.frame(final2[,c(255)])


write.csv(a1, file = "AfroTumaco1.csv")
write.csv(a2, file = "AfroTumaco2.csv")

write.csv(a3, file = "AfroTumaco3.csv")
write.csv(a4, file = "AfroTumaco4.csv")
#dep<- read_excel("Original/Arauquita.xlsx")


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)



##############################################################
#Borrar Duplicados Base de datos generales######################
##############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")

general <- read_excel("BASE DE DATOS.xlsx")
dup_Ik<-view(general[duplicated(general$Numero_identificacion_consentimiento), ])


esc2<-general %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)


a1<- data.frame(dup_Ik[,c(1:254)])
a2<- data.frame(dup_Ik[,c(255)])
a3<- data.frame(esc2[,c(1:254)])
a4<- data.frame(esc2[,c(255)])


write.csv(a1, file = "generaldup1.csv")
write.csv(a2, file = "generaldup2.csv")

write.csv(a3, file = "general1.csv")
write.csv(a4, file = "general2.csv")




cat("\f")
rm(list = ls())

##############################################################
#############################Barbacoa1######################
##############################################################

setwd("~/Documents/Investigacion/OPREC/")


#general <- read_excel("BASE DE DATOS.xlsx")
general <- read_excel("Cruces_solos/_51_ipse_oprec_escenario2__202105061310.xlsx")

#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
#paz<- read_excel(paste("Barbacoa1/LISTADO DE USUARIOS BARBACOAS 1 (3).xlsx",sep=""),1)
paz <- read_excel("Cruces_solos/Usuarios Leiva.xlsx")


##############################################################
#############################GENERAL######################
##############################################################
#names(general)
esc<- filter(general, U_codigo_municipio==52405)

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
#############################################################
paz_id<-view(paz[duplicated(paz$`N° IDENTIFICACIÓN`), ])
write.csv(paz_id, file = "leiva_dup.csv")
paz2<-paz %>%
  group_by(`N° IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`N° IDENTIFICACIÓN`))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")
#write.csv(paz_nojoin, file = "paz_nojoin.csv")
class(paz_nojoin$id)
class(no_gen$id)

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")

#paz_nojoinIA<- data.frame(paz_nojoinIA[,c(1,235,245)])
write.csv(paz_nojoinIA, file = "leiva_nojoin.csv")

final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
#final2<- rename(paz_nojoinIA, c(Numero_identificacion=id))

final1$Numero_identificacion= as.numeric(final1$Numero_identificacion)




a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])

write.csv(final1, file = "Leiva_ori.csv")
#write.csv(a1, file = "Arauca1.csv")
#write.csv(a2, file = "Arauca2.csv")


#dep<- read_excel("Original/Arauca.xlsx")


#dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

#kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)















