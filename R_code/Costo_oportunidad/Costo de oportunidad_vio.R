##############################################################################
######################Ejercicio: Calculo del costo de oportunidad#######
######################Autor: Juan Pablo Cely#################################
###############################01-12-2020####################################
####################Caña panelera por pago de servicios###############

#Santana 15686
#Chitaraque 15185
#Togui 15816
#San Jose de Pare 15664

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(readxl)
library(readr)
cat("\f")
rm(list = ls())
CNA2014_S6CUL_2013_15 <- read_csv("Documents/Investigacion/Victor/15Boyaca/15Boyaca/S06A(Cultivos)/CNA2014_S6CUL_2013_15.csv")
names(CNA2014_S6CUL_2013_15)
CNA2014<- data.frame(CNA2014_S6CUL_2013_15[,c(4,10,20)])
CNAcp<- filter(CNA2014, P_S6P46=="00180201002" )

santana<- filter(CNAcp, P_MUNIC==15686)
Chitaraque<- filter(CNAcp, P_MUNIC==15185)
Togui<- filter(CNAcp, P_MUNIC==15816)
Pare<- filter(CNAcp, P_MUNIC==15664)
CNA<-rbind(santana,Chitaraque,Togui,Pare)

CNANA<- CNA[!is.na(CNA$P_S6P59_UNIF),]

#Variables Alternativas
precio_unit<- 1500
costo_unit<-1488
ton<- 1000
inf<- 0.04
r<- inf + 0.025
b<- inf + 0.01
n<- 5
n2<- 10


p<-precio_unit*ton
c<-costo_unit*ton



CNNA2 <-CNANA %>%    mutate(beneficio= (p-c)* P_S6P59_UNIF)%>%
  mutate(CO_5años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n) ) %>%
  mutate(CO_10años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n2) )

co<- data.frame(CNNA2[,c(1,5:6)])


co2<-melt(co, id.vars = c("P_MUNIC"))


ggplot(data = co2, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Costo de oportunidad') + 
  ylab('$/hectarea/año') + guides(fill=FALSE)  +
  labs(title = "Costo de oportunidad por pago de servicios ambientales",
       caption = "Fuente: CNA (2014). DANE \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
#+ theme(axis.text.x = element_text(angle = 90))

summary(CNNA2$CO_5años) 
#quantile(CNNA2$CO_5años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))

summary(CNNA2$CO_10años) 
#quantile(CNNA2$CO_10años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))
summary(CNNA2$P_S6P59_UNIF) 



######################################
#Ganaderia por litro 
cat("\f")
rm(list = ls())


CNA2014_ENCABEZADO_15 <- read_csv("Documents/Investigacion/Victor/15Boyaca/15Boyaca/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_15.csv")
names(CNA2014_ENCABEZADO_15)



CNA2015<- data.frame(CNA2014_ENCABEZADO_15[,c(1,4,112)])


santana<- filter(CNA2015, P_MUNIC==15686)
Chitaraque<- filter(CNA2015, P_MUNIC==15185)
Togui<- filter(CNA2015, P_MUNIC==15816)
Pare<- filter(CNA2015, P_MUNIC==15664)
CNA<-rbind(santana,Chitaraque,Togui,Pare)

a<- CNA[!is.na(Pare$P_S7P85B),]
b<-filter(a, P_S7P85B!=0)

CNANA<- CNA[!is.na(CNA$P_S7P85B),]
CNANA <- filter(CNANA, P_S7P85B!=0)

r<-summary(CNANA$P_S7P85B) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
coo <- filter(CNANA, P_S7P85B<ati)
CNANA <- filter(coo, P_S7P85B>ati2)

#Variables Alternativas
precio_unit<-890
costo_unit<-870
dias<- 365
inf<- 0.04
r<- inf + 0.025
b<- inf + 0.01
n<- 5
n2<- 10


p<-precio_unit*dias
c<-costo_unit*dias



CNNA2 <-CNANA %>%    mutate(beneficio= (p-c)* P_S7P85B)%>%
  mutate(CO_5años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n) ) %>%
  mutate(CO_10años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n2) )

co<- data.frame(CNNA2[,c(2,5:6)])



co2<-melt(co, id.vars = c("P_MUNIC"))

ggplot(data = co2, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Costo de oportunidad') + 
  ylab('$/litro/año') + guides(fill=FALSE)  +
  labs(title = "Costo de oportunidad por pago de servicios ambientales",
       caption = "Fuente: CNA (2014). DANE \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
#+ theme(axis.text.x = element_text(angle = 90))

summary(co$CO_5años) 
#quantile(CNNA2$CO_5años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))

summary(co$CO_10años) 
#quantile(CNNA2$CO_10años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))
summary(CNANA$P_S7P85B)



















####################NEVADO DEL COCUY###############
####################Cultivo de papa pago de servicios###############

#El Cocuy 15244
#Guican 15332
#cubara  15223
#panqueba 15522


cat("\f")
rm(list = ls())
CNA2014_S6CUL_2013_15 <- read_csv("Documents/Investigacion/Victor/15Boyaca/15Boyaca/S06A(Cultivos)/CNA2014_S6CUL_2013_15.csv")
names(CNA2014_S6CUL_2013_15)
CNA2014<- data.frame(CNA2014_S6CUL_2013_15[,c(4,10,20)])
CNAcp<- filter(CNA2014, P_S6P46=="00151001001" )

cocuy<- filter(CNAcp, P_MUNIC==15244)
guican<- filter(CNAcp, P_MUNIC==15332)
cubara<- filter(CNAcp, P_MUNIC==15223)
panque<- filter(CNAcp, P_MUNIC==15522)
CNA<-rbind(cocuy,guican,cubara,panque)

CNANA<- CNA[!is.na(CNA$P_S6P59_UNIF),]

#Variables Alternativas
precio_unit<- 906
costo_unit<-900
ton<- 1000
inf<- 0.04
r<- inf + 0.025
b<- inf + 0.01
n<- 5
n2<- 10


p<-precio_unit*ton
c<-costo_unit*ton



CNNA2 <-CNANA %>%    mutate(beneficio= (p-c)* P_S6P59_UNIF)%>%
  mutate(CO_5años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n) ) %>%
  mutate(CO_10años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n2) )

co<- data.frame(CNNA2[,c(1,5:6)])


co2<-melt(co, id.vars = c("P_MUNIC"))


ggplot(data = co2, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Costo de oportunidad') + 
  ylab('$/hectarea/año') + guides(fill=FALSE)  +
  labs(title = "Costo de oportunidad por pago de servicios ambientales",
       caption = "Fuente: CNA (2014). DANE \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
#+ theme(axis.text.x = element_text(angle = 90))

summary(CNNA2$CO_5años) 
#quantile(CNNA2$CO_5años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))

summary(CNNA2$CO_10años) 
#quantile(CNNA2$CO_10años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))
summary(CNNA2$P_S6P59_UNIF) 









######################################
#Ganaderia por litro
cat("\f")
rm(list = ls())


CNA2014_ENCABEZADO_15 <- read_csv("Documents/Investigacion/Victor/15Boyaca/15Boyaca/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_15.csv")
names(CNA2014_ENCABEZADO_15)



CNA2015<- data.frame(CNA2014_ENCABEZADO_15[,c(1,4,112)])

 
cocuy<- filter(CNA2015, P_MUNIC==15244)
guican<- filter(CNA2015, P_MUNIC==15332)
cubara<- filter(CNA2015, P_MUNIC==15223)
panque<- filter(CNA2015, P_MUNIC==15522)
CNA<-rbind(cocuy,guican,cubara,panque)

#a<- CNA[!is.na(panque$P_S7P85B),]
#b<-filter(a, P_S7P85B!=0)


CNANA<- CNA[!is.na(CNA$P_S7P85B),]
CNANA <- filter(CNANA, P_S7P85B!=0)

r<-summary(CNANA$P_S7P85B) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
coo <- filter(CNANA, P_S7P85B<ati)
CNANA <- filter(coo, P_S7P85B>ati2)

#Variables Alternativas
precio_unit<-890
costo_unit<-870
dias<- 365
inf<- 0.04
r<- inf + 0.025
b<- inf + 0.01
n<- 5
n2<- 10


p<-precio_unit*dias
c<-costo_unit*dias



CNNA2 <-CNANA %>%    mutate(beneficio= (p-c)* P_S7P85B)%>%
  mutate(CO_5años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n) ) %>%
  mutate(CO_10años= beneficio*((1+r)/(r-b))*(1*((1+b)/(1+r))^n2) )

co<- data.frame(CNNA2[,c(2,5:6)])



co2<-melt(co, id.vars = c("P_MUNIC"))

ggplot(data = co2, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Costo de oportunidad') + 
  ylab('$/litro/año') + guides(fill=FALSE)  +
  labs(title = "Costo de oportunidad por pago de servicios ambientales",
       caption = "Fuente: CNA (2014). DANE \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
#+ theme(axis.text.x = element_text(angle = 90))

summary(co$CO_5años) 
#quantile(CNNA2$CO_5años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))

summary(co$CO_10años) 
#quantile(CNNA2$CO_10años, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))
summary(CNANA$P_S7P85B)
