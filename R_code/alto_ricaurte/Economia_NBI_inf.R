##############################################################################
######################Ejercicio: Tratamiento de datos y visualizaciones#######
######################Pobreza y economia####################################
####################NBI rural y urbano. Actividades economicas###############
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(readxl)

##############################################################################
#########################Tabla de 2 ejes: NBI URBANO Y RURAL##################
##############################################################################
NBI <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
names(NBI)
#CONVERTIR DATOS EN NUMERO
NBIDEP<- filter(NBI, `Código Departamento`==15 )

NBIDEP1<- filter(NBIDEP, `Código Entidad`==15000 )
NBIDEP2<- filter(NBIDEP, `Código Entidad`==15293)
NBIDEP3<- filter(NBIDEP, `Código Entidad`==15808)
NBIDEP4<- filter(NBIDEP, `Código Entidad`==15407)
NBIDEP5<- filter(NBIDEP, `Código Entidad`==15776)
NBIDEP6<- filter(NBIDEP, `Código Entidad`==15696)
NBIDEP7<- filter(NBIDEP, `Código Entidad`==15600)
NBIDEP8<- filter(NBIDEP, `Código Entidad`==15638)
NBIDEPt<-rbind(NBIDEP1,NBIDEP2,NBIDEP3,NBIDEP4,NBIDEP5,NBIDEP6,NBIDEP7,NBIDEP8)

NBIURB<- filter(NBIDEPt, Indicador== "Índice de Necesidades Básicas Insatisfechas - NBI - en el área urbana" )
NBIURB<- data.frame(NBIURB[,c(4,8)])
NBIURB<- rename(NBIURB, c(Urbano=Dato.Numérico))

NBIRUR<- filter(NBIDEPt, Indicador== "Índice de Necesidades Básicas Insatisfechas - NBI - en el área rural")
NBIRUR<- data.frame(NBIRUR[,c(8)])
NBIRUR<- rename(NBIRUR, c(Rural=Dato.Numérico))

NBITOTAL<- cbind(NBIURB,NBIRUR)
write.csv(NBITOTAL, file = "NBITOTAL.csv")
#NBITOTAL <- read_excel("NBITOTAL2.xlsx")

NBITOTAL <- NBITOTAL %>%
  rownames_to_column(var="Entidad")



#ggplot(NBITOTAL, aes(x=Rural, y=Urbano)) +
#  geom_point() + 
#  geom_label( 
#   data=NBITOTAL %>% filter(Urbano>0 & Rural>0), # Filter data first
#    aes(label=Entidad))

#color por  variable
#coloress<- c("hola", "nola","sola","hola", "nola","sola","hola", "nola")
#coloress<-data.frame(coloress)
#NBITOTAL<-cbind(NBITOTAL,coloress)
#Grafico alternativo
##########################NBI RURAL Y URBANO###############
NBITOTAL<- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia/Economia y NBI.xlsx")
nbaplot <- ggplot(NBITOTAL, aes(x= Rural, y = Urbano)) + 
  geom_point(color = "red", size = 3)

### geom_label_repel
nbaplot + 
  geom_label_repel(aes(label = Entidad),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+ theme_linedraw()
  #theme_light()theme_dark()theme_minimal()+
geom_text(aes(color=factor(cyl)))
#https://stackoverflow.com/questions/15624656/label-points-in-geom-point
#https://es.r4ds.hadley.nz/comunicar-con-gr%C3%A1ficos.html   Ecuaciones

miseria<- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia/miseria.xlsx")
nbaplot2 <- ggplot(miseria, aes(x= Rural, y = Urbano)) + 
  geom_point(color = "red", size = 3)

### geom_label_repel
nbaplot2 + 
  geom_label_repel(aes(label = Entidad),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+ theme_linedraw()





##############################################################################
#########################ACTIVIDAD ECONOMICA##################################
##############################################################################
economia <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim12.xlsx (1)/TerriData_Dim12.xlsx")
economiaALTO<- filter(economia, `Código Entidad`==15022 )
economiaALTO<- filter(economiaALTO, `Código Entidad`!=15000 )
economiaALTO<- data.frame(economiaALTO[,c(3:4,7:8,10)])


"Agricultura, ganadería, caza, silvicultura y pesca"
"Explotación de minas y canteras"
"Industria manufacturera" 
"Suministro de electricidad, gas y agua"
"Construcción" 
"Comercio, reparación, restaurantes y hoteles" 
"Transporte, almacenamiento y comunicaciones"
"Establecimientos financieros, seguros y otros servicios"
"Actividades de servicios sociales y personales"
"Valor agregado per cápita"
"Valor agregado"
agricultura<- filter(economiaALTO, Indicador=="Agricultura, ganadería, caza, silvicultura y pesca")
minas<-filter(economiaALTO, Indicador=="Explotación de minas y canteras")
manufactura<-filter(economiaALTO, Indicador=="Industria manufacturera")
electricidad<-filter(economiaALTO, Indicador=="Suministro de electricidad, gas y agua")
construccion<-filter(economiaALTO, Indicador=="Construcción")
comercio<-filter(economiaALTO, Indicador=="Comercio, reparación, restaurantes y hoteles")
transporte<-filter(economiaALTO, Indicador=="Transporte, almacenamiento y comunicaciones")
finanzas<-filter(economiaALTO, Indicador=="Establecimientos financieros, seguros y otros servicios")
sociales<-filter(economiaALTO, Indicador=="Actividades de servicios sociales y personales")
percapita<-filter(economiaALTO, Indicador=="Valor agregado per cápita")
valor<-filter(economiaALTO, Indicador=="Valor agregado")

sectores<-rbind(agricultura,minas,manufactura,electricidad,construccion,comercio,transporte,finanzas,sociales)

economia1<- filter(sectores, `Código.Entidad`==15293)
economia2<- filter(sectores, `Código.Entidad`==15808)
economia3<- filter(sectores, `Código.Entidad`==15407)
economia4<- filter(sectores, `Código.Entidad`==15776)
economia5<- filter(sectores, `Código.Entidad`==15696)
economia6<- filter(sectores, `Código.Entidad`==15600)
economia7<- filter(sectores, `Código.Entidad`==15638)


eco<-rbind(economia1,economia2,economia3,economia4,economia5,economia6,economia7)
write.csv(eco, file = "Economia.csv")

view(eco)#Revisar las variables que contiene


###############################NBI rural componentes####################
nbirural <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia2/nbirural.xlsx")
nbirural2<-melt(nbirural)

nbirural2 <- rename(nbirural2, c(`NBI componentes`=variable, Porcentaj=value))
nbirural2<-nbirural2%>%mutate(Porcentaje=round(Porcentaj,2))

ggplot(nbirural2, aes(x=reorder(`NBI componentes`,Porcentaje), y=Porcentaje, fill=Municipios)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=`NBI componentes`, y=Porcentaje, label = Porcentaje, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+ xlab("") + ylab("Porcentaje") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))#+coord_flip() #reorder(Indicador,value)


###############################NBI urbano componentes####################
nbiurbano <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia2/nbiurbano.xlsx")
nbiurbano2<-melt(nbiurbano)

nbiurbano2 <- rename(nbiurbano2, c(`NBI componentes`=variable, Porcentaj=value))
nbiurbano2<-nbiurbano2%>%mutate(Porcentaje=round(Porcentaj,2))

ggplot(nbiurbano2, aes(x=reorder(`NBI componentes`,Porcentaje), y=Porcentaje, fill=Municipios)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=`NBI componentes`, y=Porcentaje, label = Porcentaje, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+ xlab("") + ylab("Porcentaje") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))#+coord_flip() #reorder(Indicador,value)


###############################Actividad economica#######
##Valor agregado
acti <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia2/acti.xlsx")
acti2<-melt(acti)
acti2<-acti2%>%mutate(Porcentaje=round(value,2))
agregado<-filter(acti2, variable=="valor")
agregado <- rename(agregado, c(`Valor agregado`=Porcentaje))

ggplot(agregado, aes(x=reorder(Municipio,`Valor agregado`), y=`Valor agregado`)) +  geom_bar(position="dodge", fill="green", stat="identity") +
  geom_text(aes(x=Municipio, y=`Valor agregado`, label = `Valor agregado`, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Valor agregado (miles de millones)") + theme_minimal() +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 
##Participación
peso<-filter(acti2, variable=="peso")
peso <- rename(peso, c(`Valor peso`=Porcentaje))

ggplot(peso, aes(x=reorder(Municipio,`Valor peso`), y=`Valor peso`)) +  geom_bar(position="dodge", fill="green", stat="identity") +
  geom_text(aes(x=Municipio, y=`Valor peso`, label = `Valor peso`, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Participación (%)") + theme_minimal() +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

###############################Actividad economica por sector#######
actividad <- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Economia2/actividad.xlsx")
gacha<- filter(actividad, `Código.Entidad`==15293)

a<-ggplot(gacha, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Gachantivá")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

tinj<- filter(actividad, `Código.Entidad`==15808)

b<-ggplot(tinj, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Tinjacá")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


villa<- filter(actividad, `Código.Entidad`==15407)

c<-ggplot(villa, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Villa de Leiva")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 



suta<- filter(actividad, `Código.Entidad`==15776)

d<-ggplot(suta, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Sutamarchán")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 



santa<- filter(actividad, `Código.Entidad`==15696	)

e<-ggplot(santa, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Santa Sofia")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


raq<- filter(actividad, `Código.Entidad`==15600	)

f<-ggplot(raq, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Ráquira")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


sachi<- filter(actividad, `Código.Entidad`==15638	)

g<-ggplot(sachi, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Sáchica")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


h<-ggplot(sachi, aes(x = Año, y = Porcentaje, fill = Indicador, label = Porcentaje)) +
  geom_bar(stat = "identity") + geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +theme_void()+ theme(legend.text=element_text(size=rel(1.1)))

grid.arrange(c,f,g,d,e,b,a,h)


