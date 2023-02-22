##############################################################################
###Estudio estadistico OPREC###########################################
######################Autor: Juan Pablo Cely#################################
######################14/10/2020#################################


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


##############################################################
#############################Ipiales######################
##############################################################
cat("\f")
rm(list = ls())
setwd("~/Documents/Investigacion/OPREC/")


general<-read_csv("Ipiales/_51_ipse_oprec_escenario2__202104141633.csv")
paz<- read_excel("Ipiales/ipiales_borrar.xlsx")


##############################################################
#############################GENERAL######################
##############################################################
esc<- filter(general, U_codigo_municipio==52356)

dup_IA<-view(esc[duplicated(esc$Numero_identificacion), ])
#dup_IA2<- data.frame(dup_IA[,c(1,235,245)])

dup_Ik<-view(esc[duplicated(esc$Numero_identificacion_consentimiento), ])
#dup_Ik2<- data.frame(dup_Ik[,c(1,235,245)])
write.csv(dup_Ik, file = "ipi_dup.csv")


esc2<-esc %>%
  group_by(Numero_identificacion_consentimiento) %>%
  slice(1)

#Verificar que los que no se unieron en IK despues se unan a IA
esc3<-esc %>%
  group_by(Numero_identificacion) %>%
  slice(1)


##############################################################
#############################Tratamiento de datos#############
#############################################################
gen_r<-rename(esc2, c(id=Numero_identificacion_consentimiento))
no_gen<-rename(esc3, c(id=Numero_identificacion))
#paz_r<- rename(paz2, c(id=`NUMERO IDENTIFICACIÓN`))

#no_gen$id= as.numeric(no_gen$id)
#no_gen$id[is.na(no_gen$id)] <- 0


paz_join<-gen_r %>% semi_join(paz, by="id")
#paz_nojoin<-paz %>% anti_join(gen_r, by="id")


paz_nojoin2<-gen_r %>% anti_join(paz, by="id")


paz2join<- rename(paz_nojoin2, c(Numero_identificacion_consentimiento=id))
pazz_join<- rename(paz_join, c(Numero_identificacion_consentimiento=id))


write.csv(paz2join, file = "ipiales_ori.csv")
write.csv(pazz_join, file = "ipiales_borrado.csv")





