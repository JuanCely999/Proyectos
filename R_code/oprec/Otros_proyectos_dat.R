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
#############################Otros proyectos######################
##############################################################

setwd("~/Documents/Investigacion/OPREC/")


general <- read_excel("Otros proyectos/_51_ipse_oprec_escenario2__202103091010.xlsx")


#paz<- filter(general, U_codigo_municipio==52838)#Tuquerrez
#paz<- filter(general, U_codigo_municipio==52224)#Cuaspud
#paz<- filter(general, U_codigo_municipio==52405)#Leiva
#paz<- filter(general, U_codigo_municipio==52227)#Cumbal
paz<- filter(general, U_codigo_municipio==85125)#Hato corozal


##############################################################
#############################Municipios######################
#############################################################
#paz2_id<-view(paz[duplicated(paz$U_latitud), ])

paz_id<-view(paz[duplicated(paz$Numero_identificacion_consentimiento), ])
#write.csv(paz_id, file = "hato_sup.csv")
paz2<-paz %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)
#write.csv(paz2, file = "hato_ori.csv")











##############################################################
#############################Otra version Tumaco######################
#############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")
general <- read_excel("Otros proyectos/_51_ipse_oprec_escenario2__202103091010.xlsx")
paz <- read_excel("Otros proyectos/SSFVI Tumaco INDIVIDUAL.xlsx")

gen_r<-general %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)


paz_id<-view(paz[duplicated(paz$`N°IDENTIFICACIÓN`), ])
#write.csv(paz_id, file = "tumaco_sup.csv")

paz2<-paz %>%
  group_by(`N°IDENTIFICACIÓN`) %>%
  slice(1)


gen_r2<-rename(gen_r, c(id=Numero_identificacion_consentimiento))
paz_r<- rename(paz2, c(id=`N°IDENTIFICACIÓN`))

paz_join<-gen_r2 %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r2, by="id")


final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
final2<- rename(paz_nojoin, c(`N°IDENTIFICACIÓN`=id))

write.csv(final1, file = "tumaco_ori.csv")
write.csv(final2, file = "tumaco_nojoin.csv")












##############################################################
#############################Arauca######################
#############################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/OPREC/")
general <- read_excel("Otros proyectos/_51_ipse_oprec_escenario2__202103091010.xlsx")
paz <- read_excel(paste("N2403/USUARIOS REV 1 - ARAUCA  24-03.xlsx",sep=""),3)

genn<- filter(general, U_codigo_municipio==81001)#Arauca

gen_r<-genn %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

no_gen<-genn %>%
  group_by(Numero_identificacion) %>%
  slice(1)

paz_id<-view(paz[duplicated(paz$`N° IDENTIFICACIÓN`), ])
#write.csv(paz_id, file = "arauca_sup.csv")

paz2<-paz %>%
  group_by(`N° IDENTIFICACIÓN`) %>%
  slice(1)


gen_r2<-rename(gen_r, c(id=Numero_identificacion_consentimiento))
paz_r<- rename(paz2, c(id=`N° IDENTIFICACIÓN`))
no_gen<-rename(no_gen, c(id=Numero_identificacion))


paz_join<-gen_r2 %>% semi_join(paz_r, by="id")
paz_nojoin<-paz_r %>% anti_join(gen_r2, by="id")

paz_nojoinIA<-no_gen %>% semi_join(paz_nojoin, by="id")
write.csv(paz_nojoinIA, file = "arauca_ori2.csv")



final1<- rename(paz_join, c(Numero_identificacion_consentimiento=id))
final2<- rename(paz_nojoin, c(`N° IDENTIFICACIÓN`=id))



write.csv(final1, file = "arauca_ori.csv")
write.csv(final2, file = "arauca_nojoin.csv")



dep<- read_excel(paste("Otros proyectos/Arauca24-03.xlsx",sep=""),1)


dep2<- rename(dep, c(id=Numero_identificacion_consentimiento))

kkkk<-dep2 %>% semi_join(paz_r, by="id")

#kkkk2<-kkkk %>%
#group_by(id) %>%
#slice(1)


