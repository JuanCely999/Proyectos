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
rm(list = ls())


setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

medicamento<- read_excel("Medicamentos_cancer_keralty_version_final.xlsx")
utilizaciones <- read_excel("utilizaciones_cancer_keralty_version_final.xlsx")

#medic<- data.frame(medicamento[,c(44,65,66,26)])
medic<- data.frame(medicamento[,c(12,44,65,66,26,3)])
medic$id2<-as.numeric(medic$id2)

#utilizaciones <- rename(utilizaciones, c(id2=Numero_Identificacion_Usuario))
util2<- data.frame(utilizaciones[,c(23,35,48,65)])


########################################
corte <- read_excel("Cohorte.xlsx")
corte2<- data.frame(corte[,c(2,4)])

#corte2$id2<-as.numeric(corte2$id2)
corte22<-corte2 %>%
  group_by(id2) %>%
  slice(1)

corte22$id2<-as.numeric(corte22$id2)


opor_trata<-corte22 %>% left_join(medic, by="id2")
opor_trata<-na.omit(opor_trata)


#####################################
medic2 <- filter(opor_trata, TOTAL.CON.IVA!=0)

medic2$TOTAL.CON.IVA<-as.numeric(medic2$TOTAL.CON.IVA)
medic2<-na.omit(medic2)
#table(medic2$grupo.dx)

#medic3<- filter(medic2, grupo.dx=="CAC Cérvix" )
#medic3<- filter(medic2, grupo.dx=="CAC Colorectal" )
#medic3<- filter(medic2, grupo.dx=="CAC Estómago" )
#medic3<- filter(medic2, grupo.dx=="CAC Leucemia Linfocitica Aguda" )
#medic3<- filter(medic2, grupo.dx=="CAC Leucemia Mielocitica Aguda" )
#medic3<- filter(medic2, grupo.dx=="CAC Linfoma Hodgkin" )
#medic3<- filter(medic2, grupo.dx=="CAC Linfoma No Hodgkin" )
#medic3<- filter(medic2, grupo.dx=="CAC Mama" )
#medic3<- filter(medic2, grupo.dx=="CAC Melanoma" )
#medic3<- filter(medic2, grupo.dx=="CAC Próstata" )
#medic3<- filter(medic2, grupo.dx=="CAC Pulmón" )
#medic3<- filter(medic2, grupo.dx=="Glándulas tiroides y endocrinas" )
#medic3<- filter(medic2, grupo.dx=="Huesos y cartílagos articulares" )
#medic3<- filter(medic2, grupo.dx=="Labio, cavidad bucal y faringe" )
#medic3<- filter(medic2, grupo.dx=="Ojo, encéfalo, y otras partes del sistema nervioso" )
#medic3<- filter(medic2, grupo.dx=="Otros órganos digestivos" )
#medic3<- filter(medic2, grupo.dx=="Otros órganos genitales femeninos" )
#medic3<- filter(medic2, grupo.dx=="Otros órganos genitales masculinos" )
#medic3<- filter(medic2, grupo.dx=="Otros órganos respiratorios e intratorácicos" )
#medic3<- filter(medic2, grupo.dx=="Otros sitios, sitios mal definidos, sitios no especificados" )
#medic3<- filter(medic2, grupo.dx=="Otros tumores de la piel" )
medic3<- filter(medic2, grupo.dx=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )
#medic3<- filter(medic2, grupo.dx=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )






#medic3<- filter(medic2, grupo.dx=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )
prom_med <- medic3 %>% group_by(cancer) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))

val<-dim(prom_med)
val2<-val[1]
val3 <- rep("Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" , val2)

val4<-data_frame(val3,prom_med)

write.csv(val4, file = "22mec_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


#########Por regiones###############
prom_med5 <- medic3%>% group_by(Regional.Ciudad..Dispensacion.) %>%
  summarise(proc = round(mean(TOTAL.CON.IVA)),proc2 = sum(TOTAL.CON.IVA),proc3=(sum(contar)))

val5<-dim(prom_med5)
val25<-val5[1]
val35 <- rep("Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" , val25)

val45<-data_frame(val35,prom_med5)

write.csv(val45, file = "22reg_med_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")



#################################################
##################Utilizaciones##################
#################################################
opor_trata2<-corte22 %>% left_join(util2, by="id2")

ut_final <- filter(opor_trata2, Valor_Pagado!=0)

ut_final2<- filter(ut_final, grupo.dx=="Otros tumores de la piel" )

prom_proc <- ut_final2 %>% group_by(Servicio) %>%
  summarise(proc = round(mean(Valor_Pagado)),proc2 = sum(Valor_Pagado),proc3=(sum(definitivo_6.Columna1)))
vval<-dim(prom_proc)
vval2<-vval[1]
vval3 <- rep("Otros tumores de la piel" , vval2)

vval4<-data_frame(vval3,prom_proc)

write.csv(vval4, file = "uti_Otros tumores de la piel.csv")


#################################################3
#################################################
#################################################
cat("\f")
rm(list = ls())

a1	<- read_csv("reg_med_CAC Cérvix.csv")
a2	<- read_csv("reg_med_CAC Colorectal.csv")
a3	<- read_csv("reg_med_CAC Estómago.csv")
a4	<- read_csv("reg_med_CAC Leucemia Linfocitica Aguda.csv") 
a5	<- read_csv("reg_med_CAC CAC Leucemia Mielocitica Aguda.csv") #reg_med_CAC CAC Leucemia Mielocitica Aguda
a6	<- read_csv("reg_med_CAC CAC Linfoma Hodgkin.csv")
a7	<- read_csv("reg_med_CAC CAC Linfoma No Hodgkin.csv")
a8	<- read_csv("reg_med_CAC CAC Mama.csv")
a9	<- read_csv("reg_med_CAC CAC Melanoma.csv")
a10	<- read_csv("reg_med_CAC CAC Próstata.csv")
a11	<- read_csv("reg_med_CAC CAC Pulmón.csv")
a12	<- read_csv("reg_med_CAC Glándulas tiroides y endocrinas.csv")
a13	<- read_csv("reg_med_CAC Huesos y cartílagos articulares.csv")
a14	<- read_csv("reg_med_CAC Labio, cavidad bucal y faringe.csv")
a15	<- read_csv("reg_med_CAC Ojo, encéfalo, y otras partes del sistema nervioso.csv")
a16	<- read_csv("reg_med_CAC Otros órganos digestivos.csv")
a17	<- read_csv("reg_med_CAC Otros órganos genitales femeninos.csv")
a18	<- read_csv("reg_med_CAC Otros órganos genitales masculinos.csv")
a19	<- read_csv("reg_med_CAC Otros órganos respiratorios e intratorácicos.csv")
a20	<- read_csv("reg_med_CAC Otros sitios, sitios mal definidos, sitios no especificados.csv")
a21	<- read_csv("22reg_med_Otros tumores de la piel.csv")
a22	<- read_csv("22reg_med_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")
a23	<- read_csv("reg_med_CAC Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

aaa<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)
write.csv(aaa, file = "total medicamentos.csv")

############################################################

aa1	<- read_csv("mec_CAC Cérvix.csv")
aa2	<- read_csv("mec_CAC Colorectal.csv")
aa3	<- read_csv("mec_CAC Estómago.csv")
aa4	<- read_csv("mec_CAC Leucemia Linfocitica Aguda.csv") 
aa5	<- read_csv("mec_CAC CAC Leucemia Mielocitica Aguda.csv") #mec_CAC CAC Leucemia Mielocitica Aguda
aa6	<- read_csv("mec_CAC CAC Linfoma Hodgkin.csv")
aa7	<- read_csv("mec_CAC CAC Linfoma No Hodgkin.csv")
aa8	<- read_csv("mec_CAC CAC Mama.csv")
aa9	<- read_csv("mec_CAC CAC Melanoma.csv")
aa10	<- read_csv("mec_CAC CAC Próstata.csv")
aa11	<- read_csv("mec_CAC CAC Pulmón.csv")
aa12	<- read_csv("mec_CAC Glándulas tiroides y endocrinas.csv")
aa13	<- read_csv("mec_CAC Huesos y cartílagos articulares.csv")
aa14	<- read_csv("mec_CAC Labio, cavidad bucal y faringe.csv")
aa15	<- read_csv("mec_CAC Ojo, encéfalo, y otras partes del sistema nervioso.csv")
aa16	<- read_csv("mec_CAC Otros órganos digestivos.csv")
aa17	<- read_csv("mec_CAC Otros órganos genitales femeninos.csv")
aa18	<- read_csv("mec_CAC Otros órganos genitales masculinos.csv")
aa19	<- read_csv("mec_CAC Otros órganos respiratorios e intratorácicos.csv")
aa20	<- read_csv("mec_CAC Otros sitios, sitios mal definidos, sitios no especificados.csv")
aa21	<- read_csv("22mec_Otros tumores de la piel.csv")
aa22	<- read_csv("22mec_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")
aa23	<- read_csv("mec_CAC Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

bbb<-rbind(aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9,aa10,aa11,aa12,aa13,aa14,aa15,aa16,aa17,aa18,aa19,aa20,aa21,aa22,aa23)

write.csv(bbb, file = "total medicamentos2.csv")



caa1	<- read_csv("uti_CAC Cérvix.csv")
caa2	<- read_csv("uti_CAC Colorectal.csv")
caa3	<- read_csv("uti_CAC Estómago.csv")
caa4	<- read_csv("uti_CAC Leucemia Linfocitica Aguda.csv") 
#aa5	<- read_csv("uti_CAC CAC Leucemia Mielocitica Aguda.csv") #uti_CAC CAC Leucemia Mielocitica Aguda
#aa6	<- read_csv("uti_CAC CAC Linfoma Hodgkin.csv")
#aa7	<- read_csv("uti_CAC CAC Linfoma No Hodgkin.csv")
#aa8	<- read_csv("uti_CAC CAC Mama.csv")
#aa9	<- read_csv("uti_CAC CAC Melanoma.csv")
#aa10	<- read_csv("uti_CAC CAC Próstata.csv")
#aa11	<- read_csv("uti_CAC CAC Pulmón.csv")
#aa12	<- read_csv("uti_CAC Glándulas tiroides y endocrinas.csv")
#aa13	<- read_csv("uti_CAC Huesos y cartílagos articulares.csv")
#aa14	<- read_csv("uti_CAC Labio, cavidad bucal y faringe.csv")
#aa15	<- read_csv("uti_CAC Ojo, encéfalo, y otras partes del sistema nervioso.csv")
#aa16	<- read_csv("uti_CAC Otros órganos digestivos.csv")
#aa17	<- read_csv("uti_CAC Otros órganos genitales femeninos.csv")
#aa18	<- read_csv("uti_CAC Otros órganos genitales masculinos.csv")
#aa19	<- read_csv("uti_CAC Otros órganos respiratorios e intratorácicos.csv")
#aa20	<- read_csv("uti_CAC Otros sitios, sitios mal definidos, sitios no especificados.csv")
#aa21	<- read_csv("uti_CAC Otros tumores de la piel.csv")
#aa22	<- read_csv("uti_CAC Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")
#aa23	<- read_csv("uti_CAC Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

ccc<-rbind(caa1,caa2,caa3,caa4)
write.csv(ccc, file = "total utilizaciones.csv")



d1	<- read_csv("eg_gr_CAC-Cervix.csv")
d2	<- read_csv("eg_gr_CAC-Estomago.csv")
d3	<- read_csv("eg_gr_CAC-Hodgkin.csv")
d4	<- read_csv("eg_gr_CAC-Colorrectal.csv")
d5	<- read_csv("eg_gr_CAC-Leucemia linfoide aguda.csv")
d6	<- read_csv("eg_gr_CAC-Leucemia mieloide aguda.csv")
d7	<- read_csv("eg_gr_CAC-Mama.csv")
d8	<- read_csv("eg_gr_CAC-Melanoma.csv")
d9	<- read_csv("eg_gr_CAC-No Hodgkin.csv")
d10	<- read_csv("eg_gr_CAC-Prostata.csv")
d11	<- read_csv("eg_gr_Glándulas tiroides y endocrinas.csv")
d12	<- read_csv("eg_gr_Ojo, encéfalo, y otras partes del sistema nervioso.csv")
d13	<- read_csv("eg_gr_Órganos genitales masculinos.csv")
d14	<- read_csv("eg_gr_Otros órganos genitales femeninos.csv")
d15	<- read_csv("eg_gr_Otros tumores de la piel.csv")

ddd<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15)
write.csv(ddd, file = "total egresos.csv")




