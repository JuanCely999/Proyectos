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

cat("\f")
rm(list = ls())
total <- read_excel("Cruces_solos/_51_ipse_oprec_escenario2__202105061310.xlsx")

barba<- filter(total, U_codigo_municipio==52079)
tumac<- filter(total, U_codigo_municipio==52835)

esc<-rbind(barba,tumac)



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
write.csv(paz_id, file = "sinnombre_dup.csv")

paz2<-list_final %>%
  group_by(id) %>%
  slice(1)

esc2<-esc %>%
  group_by(Num_formulario) %>%
  slice(1)



gen_r<-rename(esc2, c(id=Num_formulario))


#paz_nojoin<-gen_r %>% anti_join(paz2, by="id")

uni<-gen_r %>% semi_join(paz2, by="id")
inst<- filter(uni, Edificacion=="InstituciÃ³n")
viv<- filter(uni, Edificacion=="Vivienda")

write.csv(inst, file = "comp_ins.csv")
write.csv(viv, file = "comp_viv.csv")



paz_nojoin2<-gen_r %>% anti_join(paz2, by="id")
inst2<- filter(paz_nojoin2, Edificacion=="InstituciÃ³n")
viv2<- filter(paz_nojoin2, Edificacion=="Vivienda")

write.csv(inst2, file = "nocomp_ins.csv")
write.csv(viv2, file = "nocomp_viv.csv")

############################################################
############################################################

ins_levantadas<-inst2 %>% semi_join(list_ins, by="id")
no_ins_levantadas<-inst2 %>% anti_join(list_ins, by="id")

write.csv(ins_levantadas, file = "levantadas_ins.csv")
write.csv(no_ins_levantadas, file = "nolevantadas_ins.csv")

no_ins_levantadas2<-list_ins %>% anti_join(inst2, by="id")
write.csv(no_ins_levantadas2, file = "revisarnolevantadas_ins.csv")



