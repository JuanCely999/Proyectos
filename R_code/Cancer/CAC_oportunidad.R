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
#############################################################################
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

corte <- read_excel("Consolidado CAC-Equipo_a16032021_Corte01012021.xlsx")
#names(corte)
#table(corte$GRUPO)

corte2 <- read_excel("Consolidado CAC-Equipo_a16032021_Corte01012021.xlsx")

#corte2<- filter(corte, GRUPO=="CAC-Cervix" )
#corte2<- filter(corte, GRUPO=="CAC-Colorrectal" )
#corte2<- filter(corte, GRUPO=="CAC-Estomago" )
#corte2<- filter(corte, GRUPO=="CAC-Hodgkin" )
#corte2<- filter(corte, GRUPO=="CAC-Leucemia linfoide aguda" )
#corte2<- filter(corte, GRUPO=="CAC-Leucemia mieloide aguda" )
#corte2<- filter(corte, GRUPO=="CAC-Mama" )
#corte2<- filter(corte, GRUPO=="CAC-Melanoma" )
#corte2<- filter(corte, GRUPO=="CAC-No Hodgkin" )
#corte2<- filter(corte, GRUPO=="CAC-Prostata" )
#corte2<- filter(corte, GRUPO=="CAC-Pulmón" )
#corte2<- filter(corte, GRUPO=="Glándulas tiroides y endocrinas" )
#corte2<- filter(corte, GRUPO=="Ojo, encéfalo, y otras partes del sistema nervioso" )
#corte2<- filter(corte, GRUPO=="Órganos genitales masculinos" )
#corte2<- filter(corte, GRUPO=="Otros órganos genitales femeninos" )
#corte2<- filter(corte, GRUPO=="Otros tumores de la piel" )
#corte2<- filter(corte, GRUPO=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )



###############Inicio del diagnostico############
corte3<- data.frame(corte2[,c(2,193)])

nulo<- filter(corte3, oportunidad_diagnostico >(-1))
diag_opor<- filter(nulo, oportunidad_diagnostico <30)
diag_inopor<- filter(nulo, oportunidad_diagnostico >30)

d_op<-dim(diag_opor)
d_op2<- d_op[1]

d_inop<-dim(diag_inopor)
d_inop2<- d_inop[1]

###############Inicio del tratamiento############
corte4<- data.frame(corte2[,c(2,194)])

nulo2<- filter(corte4, oportunidad_tratamiento >(-1))
tra_opor<- filter(nulo2, oportunidad_tratamiento <30)
tra_inopor<- filter(nulo2, oportunidad_tratamiento >30)

t_op<-dim(tra_opor)
t_op2<- t_op[1]

t_inop<-dim(tra_inopor)
t_inop2<- t_inop[1]

Categorias <- c(rep("Diagnóstico" , 1) , rep("Inicio Tto." , 1))
oportuno<-data_frame(rbind(d_op2,t_op2))
inoportuno<-data_frame(rbind(d_inop2,t_inop2))
final<-cbind(Categorias,oportuno,inoportuno)
names(final)= c("Categoria","Oportuno", "Inoportuno")

#############3
Categorias2 <- c(rep("Oportuno" , 1) , rep("Inoportuno" , 1))
n1<-data_frame(rbind(d_op2,d_inop2))
n2<-data_frame(rbind(t_op2,t_inop2))
final22<-cbind(Categorias2,n1,n2)
names(final22)= c("Categoria","Diagnostico2", "Tratamiento2")
##############

final222<- final22 %>% mutate(totH = sum(Diagnostico2)) %>%
  mutate(Diagnostico = (Diagnostico2/totH)*100)%>% mutate(totH2 = sum(Tratamiento2)) %>%
  mutate(Tratamiento = (Tratamiento2/totH2)*100) 


Diagnostico<- data.frame(final222[,c(5)])
Tratamiento<- data.frame(final222[,c(7)])
Categoria<- data.frame(final222[,c(1)])


Porcentaje<-round(Diagnostico)
Porcentaje2<-round(Tratamiento)


cegma1 = data.frame(Categoria, Porcentaje, Porcentaje2)
names(cegma1)= c("Categoria","Diagnostico", "Tratamiento")

cegma.long = melt(cegma1)

a<-ggplot(cegma.long, aes(x=reorder(variable,value), y=value, fill=Categoria)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=variable, y=value, label = percent(value/100), hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") +coord_flip()+
  labs(title = "Oportunidad en diagnóstico en tratamiento \n para cancer",
       x = "",
       y = "")
######################################
######################################
seguimiento<- data.frame(corte2[,c(15)])

Bogota <- filter(seguimiento, X14.Municipio.de.residencia=="BOGOTA")
Barranquilla <- filter(seguimiento, X14.Municipio.de.residencia=="BARRANQUILLA")
Bucaramanga <- filter(seguimiento, X14.Municipio.de.residencia=="BUCARAMANGA")
Medellin <- filter(seguimiento, X14.Municipio.de.residencia=="MEDELLIN")
Cali <- filter(seguimiento, X14.Municipio.de.residencia=="CALI")
Tunja <- filter(seguimiento, X14.Municipio.de.residencia=="TUNJA")

ciudad<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,Tunja)

tab_ciu<-data.frame(table(ciudad$X14.Municipio.de.residencia))
por_ciu<- tab_ciu %>% mutate(totf = sum(Freq)) %>%
  mutate(Porcentaje = (Freq/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")


b<-ggplot(por_ciu, aes(x=reorder(Var1,-Porcentaje), y=Porcentaje, fill=Var1)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=Var1, y=Porcentaje, label = percent(Porcentaje/100), vjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ 
  labs(title = "Seguimiento en programa",
                                                      x = "",
                                                      y = "") 




#######################################
estadios<- data.frame(corte2[,c(2,30)])

names(estadios)= c("GRUPOS","Estadio")

#estadios$Estadio<-factor(estadios$Estadio, levels=c("82","102"),labels = c("se","si"))

estadios$Estadio<-factor(estadios$Estadio, levels=c(	"0",	"1",	"2",	"3",	"4",	"5",	"6",	"7",	"8",	"9",	"10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	"31",	"32",	"33",	"94",	"95",	"98",	"99"
),labels = c(	" estadio clínico (ec) o (tumor in situ)",	" ec I o 1",	" ec IA o 1A",	" ec IA1",	" ec IA2",	" ec IB o 1b",	" ec IB1",	" ec IB2",	" ec IC o 1c",	" ec IS o 1s",	" ec II o 2",	" ec IIA o 2a",	" ec IIA1",	" ec IIA2",	" ec IIB",	" ec IIC o 2c",	" ec III o 3",	" ec IIIA o 3a",	" ec IIIB o 3b",	" ec IIIC o 3c",	" ec IV o 4",	" ec IVA o 4a",	" ec IVB o 4b",	" ec IVC o 4c",	" ec 4S (para neuroblastoma)",	" ec V o 5",	" Estadio IAB",	" ec IIIC1 o 3c1",	" ec IIIC2 o 3c2",	" ec IIID o 3d",	" ec IB3",	" ec IC1",	" ec IC2",	" ec IC3",	" Es un cáncer sólido cuyo reporte de patología no incluye la descripción de la diferenciación",	" No es sólido",	" No Aplica",	"Desconocido"
))                                                     
                                                     
tab_est<-data.frame(table(estadios$Estadio))


tab_est2<- filter(tab_est, Freq >0)

tab_est3<- tab_est2 %>% mutate(totf = sum(Freq)) %>%
  mutate(Porcentaje = (Freq/totf)*100) %>% mutate(Porcentaje=round(Porcentaje),"%")
tab_est3<- filter(tab_est3, Porcentaje >1)


c<-ggplot(tab_est3, aes(x=reorder(Var1,Porcentaje), y=Porcentaje, fill=Var1)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=Var1, y=Porcentaje, label = percent(Porcentaje/100), hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("") +coord_flip() +
labs(title = "Estadio del para cancer",
     x = "",
     y = "")

#500*500



##################################
#################################

distribucion<- data.frame(corte2[,c(4)])

names(distribucion)= c("indicador")


dist<-data.frame(table(distribucion$indicador))
dist2<- dist %>% mutate(totf = sum(Freq)) %>%
  mutate(Porcentaje = (Freq/totf)*100) %>% mutate(Porcentaje=round(Porcentaje),"%")

dist2<- filter(dist2, Freq >1)
dist2<- filter(dist2, Porcentaje >1)


d<-ggplot(dist2, aes(x=reorder(Var1,Porcentaje), y=Porcentaje, fill=Var1)) +  geom_bar(position="dodge", fill="aquamarine", stat="identity") +
  geom_text(aes(x=Var1, y=Porcentaje, label = percent(Porcentaje/100), hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("")  +coord_flip()+
  labs(title = "Distribución por tipo de tratamiento",
       x = "",
       y = "")


#######################Oportunidad de inicio de tratamiento###########
op_dist<- data.frame(corte2[,c(4,194)])

nul_op<- filter(op_dist, oportunidad_tratamiento >(-1))
tra2<- filter(nul_op, oportunidad_tratamiento <30)
tra_in2<- filter(nul_op, oportunidad_tratamiento >30)

names(tra2)= c("indicador","Valor")
names(tra_in2)= c("indicador","Valor")

tra22<-tra2 %>%
  count(indicador, sort = TRUE)

tra_in22<-tra_in2 %>%
  count(indicador, sort = TRUE)

opor_trata<-tra_in22 %>% full_join(tra22, by="indicador")
names(opor_trata)=c("indicador","Inoportuno","Oportuno")

opor_trata[is.na(opor_trata)] <- 0

cegma.long = melt(opor_trata)

e<-ggplot(cegma.long, aes(x=reorder(indicador,-value), y=value, fill=variable)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=indicador, y=value, label = value, hjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") +coord_flip()+
  labs(title = "Oportuno vs inoportuno en inicio de tratamiento \n por persona",
       x = "",
       y = "")
grid.arrange(a,e)

grid.arrange(b,c,d)
#####################################
################################
########Previos datos
###############################

#####Nuevos casos
##################Organizar fechas###################
nuevos<- data.frame(corte2[,c(2,198:200)])
#names(casos_nuevos)= c("indicador","nuevoss")


#casos_nuevos2 <- na.omit(casos_nuevos)
#casos_nuevos2 <- casos_nuevos2[!is.na(casos_nuevos2$nuevoss),]

#nuevos <- separate(casos_nuevos2, nuevoss, c("dia", "mes", "año"))

nuevos2 <- filter(nuevos, otra3==2020)
nuevos3 <- filter(nuevos2, otra2!=1)
nuevos3 <- filter(nuevos3, otra2!=2)
nuevos_casos <- filter(nuevos3, otra2!=3)
new<-dim(nuevos_casos)
new2<-new[1]
#####Dias consulta especialista promedio
consulta<- data.frame(corte2[,c(2,195)])
consulta2<- filter(consulta, Dias_consulta_especialista >(-1))

consul<-summary(consulta2$Dias_consulta_especialista)
consu<-consul[4]
round(consu)
###########################

#####Promedio hasta muerte desde el diagnostico
muerte<- data.frame(corte2[,c(2,196)])
muerte2<- filter(muerte, muerte_diagn >(-1))

muerte3<-summary(muerte2$muerte_diagn)
muerte4<-muerte3[4]
round(muerte4)

###Sobrevivir a 1 año desde diagnostico
muerte_ano<- filter(muerte2, muerte_diagn <365)
diag_mue<-dim(nulo)
diag_mue2<-diag_mue[1]

muerte_an<-dim(muerte_ano)
muerte_an2<-muerte_an[1]
porcentaje_sobre<-((muerte_an2/diag_mue2)*100)

####Mortalidad de la corte
muer_coh<-dim(muerte2)
muer_coh2<-muer_coh[1]


porcentaje_cohor<-(muer_coh2/diag_mue2)*100

#####Promedio hasta muerte desde el diagnostico
muerte_edad<- data.frame(corte2[,c(2,197)])
muerte_edad2<-summary(muerte_edad$edad_muerte)
muerte_edad3<-muerte_edad2[4]
round(muerte_edad3)

################################
########Datos puntuales
###############################

TT<-3863125#Usuarios Totales 
MM<-1822734#Usuarios Maculinos 
FF<-2040391#Usuarios Femenino 

F1<-1587532#Usuarios CAC__MAMA Población femenina <18 años  
F2<-1684819#Usuarios CAC__cervix Población femenina <15 años  
M1<-1351338#Usuarios CAC__prostata Población masculino <18 años  

################### Casos a Diciembre 2020 para cancer
muerte_dic<-dim(corte2)
muerte_dic[1]
###################Casos x 1000 afiliados para cancer
casos<-dim(corte2)
casos2<-casos[1]
(1000*casos2)/TT
###################Casos nuevos x 10000 afiliados para cancer
(10000*new2)/TT
###################Promedio para diagnostico para cancer
prom_diag<-summary(nulo$oportunidad_diagnostico) 
prom_diag2<-prom_diag[4]
round(prom_diag2)

###################Promedio para consulta medica especialista para cancer
round(consu)

###################Promedio para tratamiento desde diagnostico para cancer
prom_trat<-summary(nulo2$oportunidad_tratamiento)
prom_trat2<-prom_trat[4]
round(prom_trat2)

###################Sobrevive a un año de diagnostico para cancer
100-porcentaje_sobre

###Continuar con la mortalidad de la cohorte para cancer
porcentaje_cohor

###################Promedio hasta muerte desde diagnostico para cancer
aaa<-round(muerte4)/30
round(aaa)
###################Edad en morir########## para cancer
round(muerte_edad3)
#

