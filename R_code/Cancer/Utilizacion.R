##############################################################################
##################################Lina Cancer##
######################Autor: Juan Pablo Cely#################################
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
library(reshape2)
library(scales)
library(lubridate)

##############################################################################
##############Medicina y utilizaciones##################################
##############################################################################
cat("\f")


setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

#############################################################
utilizaciones <- read_excel("utilizaciones_cancer_keralty_version_final.xlsx")
utilizaciones <- rename(utilizaciones, c(id2=Numero_Identificacion_Usuario))

util2<- data.frame(utilizaciones[,c(12,13,23,34,35,48,55:57,58,65)])

#############################################
corte <- read_excel("Cohorte.xlsx")
corte2<- data.frame(corte[,c(2,4,5)])

corte22<-corte2 %>%
  group_by(id2) %>%
  slice(1)

medicamento<- read_excel("Medicamentos_cancer_keralty_version_final.xlsx")
#medic<- data.frame(medicamento[,c(44,65,66,26)])
medic<- data.frame(medicamento[,c(12,44,65,66,26,3)])
medic$id2<-as.numeric(medic$id2)



rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


#names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Cérvix" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Cérvix.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Cérvix.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Cérvix.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Cérvix.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Cérvix.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Cérvix.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Cérvix.csv")


##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Cérvix" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Cérvix.csv")



#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Cérvix.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Cérvix.csv")






#demo22<- filter(demo, `grupo dx`=="CAC Colorectal" )

##################################################################
##################################################################
##################################################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Colorectal" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Colorectal.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Colorectal.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Colorectal.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Colorectal.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Colorectal.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Colorectal.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Colorectal.csv")


##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Colorectal" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Colorectal.csv")



#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Colorectal.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Colorectal.csv")


#demo22<- filter(demo, `grupo dx`=="CAC Estómago" )

##################################################################
##################################################################
############################### CAC Estómago ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Estómago" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Estómago.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Estómago.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Estómago.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Estómago.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Estómago.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Estómago.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Estómago.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Estómago" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Estómago.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Estómago.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Estómago.csv")



#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Linfocitica Aguda" )



##################################################################
##################################################################
############################### CAC Leucemia Linfocitica Aguda ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Leucemia Linfocitica Aguda" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Leucemia Linfocitica Aguda.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Leucemia Linfocitica Aguda.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Leucemia Linfocitica Aguda.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Leucemia Linfocitica Aguda.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Leucemia Linfocitica Aguda.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Leucemia Linfocitica Aguda.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Leucemia Linfocitica Aguda.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Leucemia Linfocitica Aguda" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Leucemia Linfocitica Aguda.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Leucemia Linfocitica Aguda.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Leucemia Linfocitica Aguda.csv")


#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Mielocitica Aguda" )



##################################################################
##################################################################
############################### CAC Leucemia Mielocitica Aguda ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Leucemia Mielocitica Aguda" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Leucemia Mielocitica Aguda.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Leucemia Mielocitica Aguda.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Leucemia Mielocitica Aguda.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Leucemia Mielocitica Aguda.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Leucemia Mielocitica Aguda.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Leucemia Mielocitica Aguda.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Leucemia Mielocitica Aguda.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Leucemia Mielocitica Aguda" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Leucemia Mielocitica Aguda.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Leucemia Mielocitica Aguda.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Leucemia Mielocitica Aguda.csv")



#demo22<- filter(demo, `grupo dx`=="CAC Linfoma Hodgkin" )

##################################################################
##################################################################
############################### CAC Linfoma Hodgkin ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Linfoma Hodgkin" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Linfoma Hodgkin.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Linfoma Hodgkin.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Linfoma Hodgkin.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Linfoma Hodgkin.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Linfoma Hodgkin.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Linfoma Hodgkin.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Linfoma Hodgkin.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Linfoma Hodgkin" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Linfoma Hodgkin.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Linfoma Hodgkin.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Linfoma Hodgkin.csv")

#demo22<- filter(demo, `grupo dx`=="CAC Linfoma No Hodgkin" )
##################################################################
##################################################################
############################### CAC Linfoma No Hodgkin ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Linfoma No Hodgkin" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Linfoma No Hodgkin.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Linfoma No Hodgkin.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Linfoma No Hodgkin.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Linfoma No Hodgkin.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Linfoma No Hodgkin.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Linfoma No Hodgkin.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Linfoma No Hodgkin.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Linfoma No Hodgkin" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Linfoma No Hodgkin.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Linfoma No Hodgkin.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Linfoma No Hodgkin.csv")

#demo22<- filter(demo, `grupo dx`=="CAC Mama" )

##################################################################
##################################################################
############################### CAC Mama ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Mama" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Mama.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Mama.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Mama.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Mama.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Mama.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Mama.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Mama.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Mama" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Mama.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Mama.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Mama.csv")


#demo22<- filter(demo, `grupo dx`=="CAC Melanoma" )


##################################################################
##################################################################
############################### CAC Melanoma ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Melanoma" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Melanoma.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Melanoma.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Melanoma.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Melanoma.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Melanoma.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Melanoma.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Melanoma.csv")



##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Melanoma" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Melanoma.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Melanoma.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Melanoma.csv")


#demo22<- filter(demo, `grupo dx`=="CAC Próstata" )


##################################################################
##################################################################
############################### CAC Próstata ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Próstata" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Próstata.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Próstata.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Próstata.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Próstata.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Próstata.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Próstata.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Próstata.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Próstata" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Próstata.csv")



#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Próstata.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Próstata.csv")

#demo22<- filter(demo, `grupo dx`=="CAC Pulmón" )

##################################################################
##################################################################
############################### CAC Pulmón ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="CAC Pulmón" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_CAC Pulmón.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_CAC Pulmón.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_CAC Pulmón.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_CAC Pulmón.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_CAC Pulmón.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_CAC Pulmón.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_CAC Pulmón.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="CAC Pulmón" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_CAC Pulmón.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_CAC Pulmón.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_CAC Pulmón.csv")


#demo22<- filter(demo, `grupo dx`=="Glándulas tiroides y endocrinas" )

##################################################################
##################################################################
############################### Glándulas tiroides y endocrinas ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Glándulas tiroides y endocrinas" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Glándulas tiroides y endocrinas.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Glándulas tiroides y endocrinas.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Glándulas tiroides y endocrinas.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Glándulas tiroides y endocrinas.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Glándulas tiroides y endocrinas.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Glándulas tiroides y endocrinas.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Glándulas tiroides y endocrinas.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Glándulas tiroides y endocrinas" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Glándulas tiroides y endocrinas.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Glándulas tiroides y endocrinas.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Glándulas tiroides y endocrinas.csv")


#demo22<- filter(demo, `grupo dx`=="Huesos y cartílagos articulares" )

##################################################################
##################################################################
############################### Huesos y cartílagos articulares ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Huesos y cartílagos articulares" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Huesos y cartílagos articulares.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Huesos y cartílagos articulares.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Huesos y cartílagos articulares.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Huesos y cartílagos articulares.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Huesos y cartílagos articulares.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Huesos y cartílagos articulares.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Huesos y cartílagos articulares.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Huesos y cartílagos articulares" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Huesos y cartílagos articulares.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Huesos y cartílagos articulares.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Huesos y cartílagos articulares.csv")


#demo22<- filter(demo, `grupo dx`=="Labio, cavidad bucal y faringe" )

##################################################################
##################################################################
############################### Labio, cavidad bucal y faringe ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Labio, cavidad bucal y faringe" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Labio, cavidad bucal y faringe.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Labio, cavidad bucal y faringe.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Labio, cavidad bucal y faringe.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Labio, cavidad bucal y faringe.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Labio, cavidad bucal y faringe.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Labio, cavidad bucal y faringe.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Labio, cavidad bucal y faringe.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Labio, cavidad bucal y faringe" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Labio, cavidad bucal y faringe.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_med_id_Labio, cavidad bucal y faringe.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_med_id_Labio, cavidad bucal y faringe.csv")

#demo22<- filter(demo, `grupo dx`=="Ojo, encéfalo, y otras partes del sistema nervioso" )


##################################################################
##################################################################
############################### Ojo, encéfalo, y otras partes del sistema nervioso ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Ojo, encéfalo, y otras partes del sistema nervioso" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Ojo, encéfalo, y otras partes del sistema nervioso.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Ojo, encéfalo, y otras partes del sistema nervioso.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Ojo, encéfalo, y otras partes del sistema nervioso.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

##########################################################
##########################################################
#########################################################


########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Ojo, encéfalo, y otras partes del sistema nervioso" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

#demo22<- filter(demo, `grupo dx`=="Otros órganos digestivos" )

##################################################################
##################################################################
############################### Otros órganos digestivos ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros órganos digestivos" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros órganos digestivos.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros órganos digestivos.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros órganos digestivos.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros órganos digestivos.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros órganos digestivos.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros órganos digestivos.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros órganos digestivos.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros órganos digestivos" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros órganos digestivos.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros órganos digestivos.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros órganos digestivos.csv")


#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales femeninos" )

##################################################################
##################################################################
############################### Otros órganos genitales femeninos ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros órganos genitales femeninos" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros órganos genitales femeninos.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros órganos genitales femeninos.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros órganos genitales femeninos.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros órganos genitales femeninos.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros órganos genitales femeninos.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros órganos genitales femeninos.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros órganos genitales femeninos.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros órganos genitales femeninos" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros órganos genitales femeninos.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros órganos genitales femeninos.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros órganos genitales femeninos.csv")


#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales masculinos" )


##################################################################
##################################################################
############################### Otros órganos genitales masculinos ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros órganos genitales masculinos" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros órganos genitales masculinos.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros órganos genitales masculinos.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros órganos genitales masculinos.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros órganos genitales masculinos.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros órganos genitales masculinos.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros órganos genitales masculinos.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros órganos genitales masculinos.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros órganos genitales masculinos" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros órganos genitales masculinos.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros órganos genitales masculinos.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros órganos genitales masculinos.csv")

#demo22<- filter(demo, `grupo dx`=="Otros órganos respiratorios e intratorácicos" )

##################################################################
##################################################################
############################### Otros órganos respiratorios e intratorácicos ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros órganos respiratorios e intratorácicos" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros órganos respiratorios e intratorácicos.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros órganos respiratorios e intratorácicos.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros órganos respiratorios e intratorácicos.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros órganos respiratorios e intratorácicos.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros órganos respiratorios e intratorácicos.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros órganos respiratorios e intratorácicos.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros órganos respiratorios e intratorácicos.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros órganos respiratorios e intratorácicos" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros órganos respiratorios e intratorácicos.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros órganos respiratorios e intratorácicos.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros órganos respiratorios e intratorácicos.csv")

#demo22<- filter(demo, `grupo dx`=="Otros sitios, sitios mal definidos, sitios no especificados" )

##################################################################
##################################################################
############################### Otros sitios, sitios mal definidos, sitios no especificados ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros sitios, sitios mal definidos, sitios no especificados" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros sitios, sitios mal definidos, sitios no especificados.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros sitios, sitios mal definidos, sitios no especificados.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros sitios, sitios mal definidos, sitios no especificados.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros sitios, sitios mal definidos, sitios no especificados.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros sitios, sitios mal definidos, sitios no especificados.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros sitios, sitios mal definidos, sitios no especificados.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros sitios, sitios mal definidos, sitios no especificados.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros sitios, sitios mal definidos, sitios no especificados" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros sitios, sitios mal definidos, sitios no especificados.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros sitios, sitios mal definidos, sitios no especificados.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros sitios, sitios mal definidos, sitios no especificados.csv")

#demo22<- filter(demo, `grupo dx`=="Otros tumores de la piel" )

##################################################################
##################################################################
############################### Otros tumores de la piel ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros tumores de la piel" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros tumores de la piel.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros tumores de la piel.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros tumores de la piel.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros tumores de la piel.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros tumores de la piel.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros tumores de la piel.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros tumores de la piel.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros tumores de la piel" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros tumores de la piel.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros tumores de la piel.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros tumores de la piel.csv")

#demo22<- filter(demo, `grupo dx`=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )


##################################################################
##################################################################
############################### Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


#demo22<- filter(demo, `grupo dx`=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )


##################################################################
##################################################################
############################### Tejidos mesoteliales (excepto pulmón) y tejidos blandos ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")



#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

#demo22<- filter(demo, `grupo dx`=="Tumores secundarios" )

##################################################################
##################################################################
############################### Tumores secundarios ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Tumores secundarios" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Tumores secundarios.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Tumores secundarios.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Tumores secundarios.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Tumores secundarios.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Tumores secundarios.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Tumores secundarios.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Tumores secundarios.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Tumores secundarios" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Tumores secundarios.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Tumores secundarios.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Tumores secundarios.csv")


#demo22<- filter(demo, `grupo dx`=="Vías urinarias" )
##################################################################
##################################################################
############################### Vías urinarias ###################################
##################################################################
rm(list=ls()[! ls() %in% c("utilizaciones","util2","corte22","medicamento","medic")])


###################################################################names(corte22)= c("grupo","id2")


corte22$id2<-as.numeric(corte22$id2)

opor_trata<-corte22 %>% left_join(util2, by="id2")


util3 <- filter(opor_trata, Valor_Pagado!=0)

util3$Valor_Pagado<-as.numeric(util3$Valor_Pagado)
#medic2<-na.omit(medic2)
#table(util3$grupo.dx)

util4<- filter(util3, grupo.dx=="Vías urinarias" )
prom_med <- util4 %>% group_by(Grupo_Principal) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med<-arrange(prom_med, desc(proc2))
write.csv(prom_med, file = "pri_Vías urinarias.csv")


prom_med1 <- util4 %>% group_by(Grupo_1) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med1<-arrange(prom_med1, desc(proc2))
write.csv(prom_med1, file = "gr1_Vías urinarias.csv")


prom_med2 <- util4 %>% group_by(Grupo_2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med2<-arrange(prom_med2, desc(proc2))
write.csv(prom_med2, file = "gr2_Vías urinarias.csv")

prom_med3 <- util4 %>% group_by(Grupo_3) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
prom_med3<-arrange(prom_med3, desc(proc2))
write.csv(prom_med3, file = "gr3_Vías urinarias.csv")

usu_med <- util4 %>% group_by(id2) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

usu_med2<-arrange(usu_med, desc(proc2))
usu_med3<-usu_med2[1:10,]
write.csv(usu_med3, file = "id_Vías urinarias.csv")

pre_med <- util4 %>% group_by(Nombre.del.prestador) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

pre_med2<-arrange(pre_med, desc(proc2))
#pre_med3<-pre_med2[1:10,]
write.csv(pre_med2, file = "prest_Vías urinarias.csv")


suc_med <- util4 %>% group_by(SUCURSAL_IPS) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

suc_med2<-arrange(suc_med, desc(proc2))
#suc_med3<-suc_med2[1:10,]
write.csv(suc_med2, file = "suc_Vías urinarias.csv")

##########################################################
##########################################################
#########################################################

########################################

opor_trata2<-corte22 %>% left_join(medic, by="id2")
opor_trata2<-na.omit(opor_trata2)


#####################################
medic2 <- filter(opor_trata2, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)

medic3<- filter(medic2, grupo.dx=="Vías urinarias" )
u_med <- medic3 %>% group_by(id2) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))
u_med2<-arrange(u_med, desc(proc2))
u_med3<-u_med2[1:10,]
write.csv(u_med3, file = "med_id_Vías urinarias.csv")

#############################################################3
a_med <- filter(util4, Lugar=="AMBULATORIA")
h_med <- filter(util4, Lugar=="HOSPITALIZACION")


ambu_med <- a_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

ambu_med2<-arrange(ambu_med, desc(proc2))
write.csv(ambu_med2, file = "ambu_Vías urinarias.csv")

hosp_med <- h_med %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))

hosp_med2<-arrange(hosp_med, desc(proc2))
write.csv(hosp_med2, file = "hosp_Vías urinarias.csv")



