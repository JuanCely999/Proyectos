##############################################################################
###Proyecto de colciencias: Estadisticas de la bibliometria en WOS###########
##################Graficas apiladas en los temas del turismo##################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
library(ggplot2)
library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(readxl)
library(plyr)

###############Base de datos de ejemplo###
###https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
####otra opcion##### https://codeday.me/es/qa/20181224/44242.html

#################################################
#################################################original#######################

base <- read_excel("baseR.xlsx")
data <- data.frame(base)
#########################ADVERTENCIA###########################
######################En este ejercicio no se reemplaza a menos que se necesite######
#data1<-data
#eliminar.cero=function(valores){
 # ifelse(is.na(valores),0,valores)}
#datos.originales=data.frame(sapply(data1,eliminar.cero))

########Opcion aunque carece del total de los ejes x#########
ggplot(data, aes(x = A?o, y = valores, fill = Categorias, label = valores)) +
  geom_bar(stat = "identity") 

############2 opcionigualmente carece del total de los ejes x aunque ahora con 180?
ggplot(data, aes(fill=Categorias, y=valores, x=A?o)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90))     # Reduzco el tama?o a 6 puntos 


######################################Datos iniciales
#####################################
recopilacion <- read_excel("baseR.xlsx")
names(recopilacion)
#data1<-recopilacion
#eliminar.cero=function(valores){
 # ifelse(is.na(valores),0,valores)}
#datos.originales=data.frame(sapply(data1,eliminar.cero))
Valores<-recopilacion$valores

Categorias <- c(rep("cities" , 34) , rep("marine land" , 34) , rep("land" , 34)
            , rep("ocean" , 34), rep("geoforum" , 34), rep("rural" , 34)
            , rep("turismo" , 34), rep("social" , 34), rep("produccion" , 34)
            , rep("geografia" , 34)
            ) ######la columna mantiene los valores pegados
A?os <- rep(c("1986" , "1987" , "1988" , "1989" , "1990" , "1991" , "1992"
              , "1993" , "1994" , "1995" , "1996" , "1997" , "1998" , "1999"
              , "2000" , "2001" , "2002" , "2003" , "2004" , "2005" , "2006"
              , "2007" , "2008" , "2009" , "2010" , "2011" , "2012"
              , "2013" , "2014" , "2015" , "2016" , "2017" , "2018" , "2019") , 10)#los valores permanecen despegados

final <- data.frame(A?os,Categorias,Valores)
View(final)

########GRAFICO ELEGIDO
ggplot(final, aes(fill=Categorias, y=Valores, x=A?os)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) 


############Grafico con otro color"no"
ggplot(final, aes(fill=Categorias, y=Valores, x=A?os, label=Valores)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T)+
  theme_ipsum() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90))


#############Grafica con etiquetas########
ggplot(final, aes(x = A?os, y = Valores, fill = Categorias, label = Valores)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 2)) +
  theme(axis.text.x = element_text(angle = 90))




