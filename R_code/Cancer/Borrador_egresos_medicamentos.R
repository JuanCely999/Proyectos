###############################03-04-2021####################################
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

library(scales)
library(lubridate)
#############################################################################
cat("\f")
rm(list = ls())


setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

estancia<- read_excel("egresos_pacientes_cancer_2020 (1)VF.xlsx")
cie10<- read_excel("CIE10 CÁNCER.xlsx")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Cervix" )


##################################################################
##################################################################
############################### CAC-Cervix###################################
##################################################################



rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Cervix" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Cervix.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Estomago" )

##################################################################
##################################################################
############################### CAC-Estomago ########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Estomago" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Estomago.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Hodgkin" )

##################################################################
##################################################################
############################### CAC-Hodgkin########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Hodgkin" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Hodgkin.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Colorrectal" )

##################################################################
##################################################################
############################### CAC-Colorrectal########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Colorrectal" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Colorrectal.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia linfoide aguda" )

##################################################################
##################################################################
############################### CAC-Leucemia linfoide aguda########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia linfoide aguda" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Leucemia linfoide aguda.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia mieloide aguda" )
##################################################################
##################################################################
############################### CAC-Leucemia mieloide aguda########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Leucemia mieloide aguda" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Leucemia mieloide aguda.csv")

#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Mama" )

##################################################################
##################################################################
############################### CAC-Mama########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Mama" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Mama.csv")

#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Melanoma" )

##################################################################
##################################################################
############################### CAC-Melanoma########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Melanoma" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Melanoma.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-No Hodgkin" )


##################################################################
##################################################################
############################### CAC-No Hodgkin########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-No Hodgkin" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-No Hodgkin.csv")



#demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Prostata" )

##################################################################
##################################################################
############################### CAC-Prostata########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="CAC-Prostata" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_CAC-Prostata.csv")



#demo22<- filter(id_cancer, `GRUPO DX`=="Glándulas tiroides y endocrinas" )

##################################################################
##################################################################
############################### Glándulas tiroides y endocrinas########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="Glándulas tiroides y endocrinas" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_Glándulas tiroides y endocrinas.csv")




#demo22<- filter(id_cancer, `GRUPO DX`=="Ojo, encéfalo, y otras partes del sistema nervioso")

##################################################################
##################################################################
############################### Ojo, encéfalo, y otras partes del sistema nervioso########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="Ojo, encéfalo, y otras partes del sistema nervioso" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_Ojo, encéfalo, y otras partes del sistema nervioso.csv")


#demo22<- filter(id_cancer, `GRUPO DX`=="Órganos genitales masculinos")

##################################################################
##################################################################
############################### Órganos genitales masculinos########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="Órganos genitales masculinos" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_Órganos genitales masculinos.csv")

#demo22<- filter(id_cancer, `GRUPO DX`=="Otros órganos genitales femeninos" )

##################################################################
##################################################################
############################### Otros órganos genitales femeninos########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="Otros órganos genitales femeninos" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_Otros órganos genitales femeninos.csv")

#demo22<- filter(id_cancer, `GRUPO DX`=="Otros tumores de la piel" )

##################################################################
##################################################################
############################### Otros tumores de la piel########################
##################################################################

rm(list=ls()[! ls() %in% c("utilizaciones","estancia","cie10")])

id_cancer<-cie10 %>% left_join(estancia, by="id")

demo22<- filter(id_cancer, `GRUPO DX`=="Otros tumores de la piel" )

demo2<- data.frame(demo22[,c(3,13,54,83,130,146)])
demo2$DIA2 <- as.numeric(demo2$DIA2)
demo2$VALOR.FACTURA <- as.numeric(demo2$VALOR.FACTURA)

demo2 <- na.omit(demo2)

esta_prom <- demo2 %>% group_by(GRUPO_UNO) %>% 
  summarise(proc = round(mean(VALOR.FACTURA)),proc2 = sum(VALOR.FACTURA),est = round(mean(DIA2)),proc3=(sum(contar)))
egre_med2<-arrange(esta_prom, desc(proc2))
write.csv(egre_med2, file = "egre_id_Otros tumores de la piel.csv")


