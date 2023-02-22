##############################################################################
##################################Lina Cancer##
######################Autor: Juan Pablo Cely#################################
###############################03-04-2021####################################
rm(list=ls())
library(munsell)
library(curl)
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
library(lubridate)

cat("\f")
rm(list = ls())
##################Hace falta estancia valor y por usuarios mas costosos

setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

estancia<- read_excel("egresos_pacientes_cancer_2020 (1)VF.xlsx")
cie10<- read_excel("CIE10 CÁNCER.xlsx")


id_cancer<-cie10 %>% left_join(estancia, by="id")
#costos<- data.frame(id_cancer[,c(3,13,54,83,130)])
#write.csv(costos, file = "costos_hospitalarios.csv")
#table(id_cancer$`GRUPO DX`)

#demo22<- filter(id_cancer, `GRUPO DX`!="Otros tumores de piel" )#Solo para analisis de cancer general
####################################

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Cervix" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Estomago" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Hodgkin" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Colorrectal" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia linfoide aguda" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia mieloide aguda" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Mama" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Melanoma" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-No Hodgkin" )
#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Prostata" )
#demo22<- filter(id_cancer, `GRUPO DX`=="Glándulas tiroides y endocrinas" )
#demo22<- filter(id_cancer, `GRUPO DX`=="Ojo, encéfalo, y otras partes del sistema nervioso" )
#demo22<- filter(id_cancer, `GRUPO DX`=="Órganos genitales masculinos" )
#demo22<- filter(id_cancer, `GRUPO DX`=="Otros órganos genitales femeninos" )
#demo22<- filter(id_cancer, `GRUPO DX`=="Otros tumores de la piel" )


demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% summarise(est = round(mean(DIA2)))

esta_prom <- na.omit(esta_prom)

a<-ggplot(esta_prom, aes(x=reorder(GRUPO_UNO,est), y=est, fill=GRUPO_UNO)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=GRUPO_UNO, y=est, label = est, hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("")  +coord_flip()+
  labs(title = "Promedio dias estancia de \n para cancer por especialidad",
       x = "",
       y = "")

cont_grup<-demo2 %>%
  count(GRUPO_UNO, sort = TRUE)

cont_grup <- na.omit(cont_grup)

tab_est3<- cont_grup %>% mutate(totf = sum(n)) %>%
  mutate(Porcentaje = (n/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")

tab_est3<- filter(tab_est3, Porcentaje >1)

b<-ggplot(tab_est3, aes(x=reorder(GRUPO_UNO,Porcentaje), y=Porcentaje, fill=GRUPO_UNO)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=GRUPO_UNO, y=Porcentaje, label = percent(Porcentaje/100), hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("")  +coord_flip()+
  labs(title = "Distribución servicios en para cancer",
       x = "",
       y = "")

#######################3
cont_grup2<-demo2 %>%
  count(PROCEDIMIENTO.PRINCIPAL, sort = TRUE)

cont_grup2 <- na.omit(cont_grup2)

tab_est32<- cont_grup2 %>% mutate(totf = sum(n)) %>%
  mutate(Porcentaje = (n/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")


#tab_est32<- filter(tab_est32, PROCEDIMIENTO.PRINCIPAL!="RESECCION DE TUMOR RETROPERITONEAL CON DISECCION DE ESTRUCTURAS VASCULARES U ORGANOS RETROPERITONEALES VIA ABIERTA - PAQUETE" )

tab_est322<-tab_est32[1:10,]


c<-ggplot(tab_est322, aes(x=reorder(PROCEDIMIENTO.PRINCIPAL,Porcentaje), y=Porcentaje, fill=PROCEDIMIENTO.PRINCIPAL)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=PROCEDIMIENTO.PRINCIPAL, y=Porcentaje, label = percent(Porcentaje/100), hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("")  +coord_flip()+
  labs(title = "Procedimientos en \n para cancer",
       x = "",
       y = "")
grid.arrange(a,b)#800*600
grid.arrange(c)#1000*600

##############################################

demo2$VALOR.FACTURA<-as.numeric(demo2$VALOR.FACTURA)
demo2<-na.omit(demo2)
prom_dem <- demo2 %>% group_by(PROCEDIMIENTO.PRINCIPAL) %>%
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),proc3=(sum(contar)))
val<-dim(prom_dem)
val2<-val[1]
val3 <- rep("para cancer" , val2)
val4<-data_frame(val3,prom_dem)

val4<-na.omit(val4)

#write.csv(val4, file = "eg_pr_para cancer.csv")

#################################################

prom_dem2 <- demo2 %>% group_by(GRUPO_UNO) %>%
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),proc3=(sum(contar)))
vall<-dim(prom_dem2)
vall2<-vall[1]
vall3 <- rep("para cancer" , vall2)
vall4<-data_frame(vall3,prom_dem2)

#write.csv(vall4, file = "eg_gr_para cancer.csv")

