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
#############################Barbacoa1######################
##############################################################

setwd("~/Documents/Investigacion/OPREC/")


general <- read_excel("Barbacoa1/BASE DE DATOS BARBACOAS1 (1).xlsx")

#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("Barbacoa1/LISTADO DE USUARIOS BARBACOAS 1 (3).xlsx",sep=""),1)


##############################################################
#############################GENERAL######################
##############################################################
#names(general)
#esc<- filter(general, U_codigo_municipio==52079)
esc<-general


dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
#write.csv(dup_IA, file = "barbacoa1generaldup.csv")

esc2<-esc %>%
  group_by(Numero_identificacion) %>%
  slice(1)



##############################################################
#############################################################
paz_id<-view(paz[duplicated(paz$Numero_identificacion_consentimiento), ])
#write.csv(paz_id, file = "barbacoa1dup.csv")
paz2<-paz %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion))
#no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=Numero_identificacion_consentimiento))

no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")

final1<- rename(paz_join, c(Numero_identificacion=id))
final2<- rename(paz_nojoin, c(Numero_identificacion_consentimiento=id))


a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])


write.csv(a1, file = "barba_orig.csv")
write.csv(a2, file = "barba_orig2.csv")

write.csv(final2, file = "barba_nojoin.csv")






##############################################################
##############################################################
##############################################################
#############################Barbacoa2######################
##############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")


general <- read_excel("Barbacoa1/BASE DE DATOS BARBACOAS1 (1).xlsx")

#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),1)
#paz<- read_excel(paste("USUARIOS COMUNIDADES INDIGENAS PAZ DE ARIPORO (1).xlsx",sep=""),2)
paz<- read_excel(paste("Barbacoa1/107.1. USUARIOS BARBACOAS VF.xlsx",sep=""),2)


##############################################################
#############################GENERAL######################
##############################################################
#names(general)
#esc<- filter(general, U_codigo_municipio==52079)
esc<-general


dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
#write.csv(dup_IA, file = "barbacoa1generaldup.csv")

esc2<-esc %>%
  group_by(Numero_identificacion) %>%
  slice(1)


##############################################################
#############################################################
paz_id<-view(paz[duplicated(paz$`N° IDENTIFICACIÓN`), ])
write.csv(paz_id, file = "barbacoa12dup.csv")

paz2<-paz %>%
  group_by(`N° IDENTIFICACIÓN`) %>%
  slice(1)
#write.csv(paz2, file = "paz2.csv")

gen_r<-rename(esc2, c(id=Numero_identificacion))
#no_gen<-rename(esc3, c(id=Numero_identificacion))
paz_r<- rename(paz2, c(id=`N° IDENTIFICACIÓN`))

#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz_r, by="id")

paz_nojoin<-paz_r %>% anti_join(gen_r, by="id")

final1<- rename(paz_join, c(Numero_identificacion=id))

a1<- data.frame(final1[,c(1:254)])
a2<- data.frame(final1[,c(255)])


write.csv(a1, file = "barba2_orig.csv")
write.csv(a2, file = "barba2_orig2.csv")




general2 <- read_excel("BASE DE DATOS.xlsx")

gen2<-general2 %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)


gen2_r<-rename(gen2, c(id=Numero_identificacion_consentimiento))

paz_2join<-gen2_r %>% semi_join(paz_nojoin, by="id")

final2<- rename(paz_2join, c(Numero_identificacion_consentimiento=id))

a3<- data.frame(final2[,c(1:254)])
a4<- data.frame(final2[,c(255)])


write.csv(a3, file = "barba21_orig.csv")
write.csv(a4, file = "barba21_orig2.csv")



paz_2nojoin<-paz_nojoin %>% anti_join(gen2_r, by="id")

genIA<-general2 %>%
  group_by(Numero_identificacion) %>%
  slice(1)

no_gen<-rename(genIA, c(id=Numero_identificacion))

no_gen$id= as.numeric(no_gen$id)

paz_2joinIA<-no_gen %>% semi_join(paz_2nojoin, by="id")

paz_2nojoinIA<-paz_2nojoin %>% anti_join(no_gen, by="id")

final3<- rename(paz_2joinIA, c(Numero_identificacion=id))


a5<- data.frame(final3[,c(1:254)])
a6<- data.frame(final3[,c(255)])


write.csv(a4, file = "barba22_orig.csv")
write.csv(a5, file = "barba22_orig2.csv")



