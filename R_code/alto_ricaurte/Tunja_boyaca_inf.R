#############################################################################
###Calidad internet en Tunja######################
####################Estadisticas y Graficas de cajas MINTIC#########
######################Autor: Juan Pablo Cely#################################
###############################07-04-2020####################################
rm(list=ls())
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

internet <- read_excel("Documents/Investigacion/Alto ricaurte/Complementos/Boxplot calidad internet en Tunja.xlsx")



r<-summary(internet$BAJADA) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
internet2 <- filter(internet, BAJADA<ati)
internet2 <- filter(internet2, BAJADA>ati2)



ggplot(data = internet2, aes(x = SEGMENTO, y = BAJADA)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = SEGMENTO), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Segmento') + 
  ylab('Mbps') + guides(fill=FALSE)  +
  labs(title = "Velocidad de bajada segmentado en Tunja (2017-2019)",
       caption = "Fuente: MINTIC. Informe trimestral de las TIC (2019) \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(axis.text.x = element_text(angle = 90))

#________________________________#
r2<-summary(internet$SUBIDA) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
internet2 <- filter(internet, SUBIDA<ati)
internet2 <- filter(internet2, SUBIDA>ati2)



ggplot(data = internet2, aes(x = SEGMENTO, y = SUBIDA)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = SEGMENTO), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Segmento') + 
  ylab('Mbps') + guides(fill=FALSE)  +
  labs(title = "Velocidad de subida segmentado en Tunja (2017-2019)",
       caption = "Fuente: MINTIC. Informe trimestral de las TIC (2019) \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(axis.text.x = element_text(angle = 90))

a<-filter(internet,  SEGMENTO=="Corporativo")
summary(a)

b<-filter(internet,  SEGMENTO=="Residencial - Estrato 1")
summary(b)

c<-filter(internet,  SEGMENTO=="Residencial - Estrato 2")
summary(c)

d<-filter(internet,  SEGMENTO=="Residencial - Estrato 3")
summary(d)

e<-filter(internet,  SEGMENTO=="Residencial - Estrato 4")
summary(e)

f<-filter(internet,  SEGMENTO=="Residencial - Estrato 5")
summary(f)

g<-filter(internet,  SEGMENTO=="Residencial - Estrato 6")
summary(g)

h<-filter(internet,  SEGMENTO=="Residencial - Estrato 1")
summary(h)

i<-filter(internet,  SEGMENTO=="Sin estratificar")
summary(i)


#---------------------Mapa de  ocupadas formalmente con respecto a la población total------------#


directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)
boyaca_mapa <- st_read(dsn = "./15_BOYACA/ADMINISTRATIVO/", layer = "MGN_MPIO_POLITICO")
colombia_ocupa <- read_excel("~/Documents/Investigacion/covid19/Empresas en Boyaca/Datos/TerriData_Dim16_Sub9_Var1.xlsx/TerriData_Dim16_Sub9_Var1.xlsx")
names(colombia_ocupa)
boyaca<- filter(colombia_ocupa, `Código Departamento`==15 )
boyaca<- filter(boyaca, `Año`==2016 )
names(boyaca)
boyaca2 = rename(boyaca, c(MPIO_CCDGO=`Código Entidad`, Porcentaje=`Dato Numérico`))
boyaca2<- data.frame(boyaca2[,c(1:4,8)])
#write.csv(boyaca2, file = "boyaca_trab.csv")
#boyaca3 <- read.csv("~/Documents/Investigacion/Mapas Pemp/boyaca_trab.csv")
boyaca_trab <- boyaca_mapa %>% 
  left_join(boyaca2)

#Adicionar nombres
boyaca_trab <- boyaca_trab %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                  st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                     2))


#Mapa de calor por municipio
ggplot(boyaca_trab) +
  geom_sf(aes(fill =Porcentaje))+ xlab("") + ylab("") +
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  labs(title = "Porcentaje de personas ocupadas formalmente con respecto a la población total, 2016",
       caption = "Fuente: DNP a partir de FILCO - Ministerio del Trabajo. \n Elaborado por: Juan Pablo Cely") +
  annotation_scale() +
  annotation_north_arrow(location='tl')+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = MPIO_CNMBR), size = 2, min.segment.length = 0) +
coord_sf(crs = st_crs(boyaca_trab), datum = NA)+
  theme_void() #Quitar las coordenadas https://arcruz0.github.io/libroadp/mapas.html para autocorrelación espacial
#__________________________________________#
#__________________________________________#
#__________________________________________#
#__________________________________________#
#__________________________________________#
####Serie de tiempo ocupacion informal#####
#__________________________________________#
#__________________________________________#
#__________________________________________#

informal <- read_excel("~/Documents/Investigacion/Alto ricaurte/Empresas en Boyaca/Datos/informal.xlsx")
df <- informal[informal$`Ocupación informal` %in% c("Tunja","23 ciudades y áreas metropolitanas"), ]
view(df)
#df <- df[lubridate::year(df$etiq) %in% c(2007:2019), ]
# labels and breaks for X axis text
#brks <- df$date[seq(1, length(df$date), 12)]
#lbls <- lubridate::year(brks)

# labels and breaks for X axis text
#brks <- df$date[seq(1, length(df$date), 12)]
#lbls <- lubridate::year(brks)

#http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
ggplot(df, aes(x = date, y = informal)) + 
  geom_line(aes(color = `Ocupación informal`), size = 1) + xlab("") + ylab("Porcentaje de ocupados informales") +
  scale_color_manual(values = c("#00AFBB","#E7B800")) +
  labs(title = "Ocupación informal (2007-2019)",
       caption = "Fuente: GEIH Mercado laboral - empleo informal y seguridad social  \n Elaborado por: Juan Pablo Cely") +
  theme_minimal() +theme(legend.position='bottom') + stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) 







#__________________________________________#
#__________________________________________#

####Serie dias de efectivo de una empresa#####
#__________________________________________#
#__________________________________________#
#__________________________________________#
empresas <- read_excel("~/Documents/Investigacion/Alto ricaurte/Empresas en Boyaca/Datos/empresas.xlsx")

ggplot(data = empresas)+ 
  geom_point(mapping = aes(x = Dias, y = `Empresarios-Empleos`, color = Variable))+geom_vline( xintercept=20)+ 
  geom_vline( xintercept=40)+  geom_vline( xintercept=70)+ 
geom_smooth(mapping = aes(x = Dias, y = `Empresarios-Empleos`, linetype = Variable, color = Variable))+
  annotate("text", x=20, y=12000, label= "Microempresas") + 
  annotate("text", x=20, y=11500, label= "(13 dias)") +
  annotate("text", x = 30, y=9000, label = "Pequeñas ") + 
  annotate("text", x = 30, y=8500, label = "(30 dias) ") + 
  annotate("text", x = 60, y=6000, label = "Medianas") + 
  annotate("text", x = 60, y=5500, label = "(60 dias)") + 
  annotate("text", x = 90, y=6000, label = "Grandes") +
  annotate("text", x = 90, y=5500, label = "(100 dias)") + 
  theme(legend.position='bottom')+
  labs(title = "Días de amortiguación de efectivo según el tamaño de la empresa en Tunja (2019)
", caption = "Fuente: Cámara de Comercio de Tunja  \n Elaborado por: Juan Pablo Cely") 
  


#__________________________________________#
#__________________________________________#

####Serie de tiempo por años entre jurisdicciones#####
#__________________________________________#
#__________________________________________#
#__________________________________________#
sucursales <- read_excel("~/Documents/Investigacion/Alto ricaurte/Empresas en Boyaca/Datos/sucursales.xlsx")
ggplot(data = sucursales)+ 
  geom_point(mapping = aes(x = Año, y = `Número de empresas`, color = Jurisdicciones))+ 
  geom_smooth(mapping = aes(x = Año, y = `Número de empresas`, color = Jurisdicciones))+
  theme(legend.position='bottom')+
  labs(title = "Empresas generadoras de empleo formal por cada 10.000 habitantes (2010-2016)
", caption = "Fuente: DNP a partir de FILCO - Ministerio del Trabajo  \n Elaborado por: Juan Pablo Cely") 

  
  
#__________________________________________#
#__________________________________________#
#__________________________________________#
#__________________________________________#
#__________________________________________#
####Serie de tiempo tasa de desempleo EXTENSION#####
#__________________________________________#
#__________________________________________#
#__________________________________________#
#Ampliar rango de ejes x
desempleo <- read_excel("~/Documents/Investigacion/covid19/Empresas en Boyaca/Datos/desempleo.xlsx")
df <- desempleo[desempleo$Ciudades %in% c("Tunja","23 ciudades y áreas metropolitanas"), ]
ggplot(df, aes(date, y = Desempleo)) + 
  geom_line(aes(color = Ciudades), size = 1) + xlab("") + ylab("Tasa de desempleo") +
  scale_color_manual(values = c("#00AFBB","#E7B800")) +
  theme_minimal() +theme(legend.position='bottom') + stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + scale_x_continuous(breaks=pretty(df$date, n = 10), name="year")

#####MEJOR OPCION
desempleo <- read_excel("~/Documents/Investigacion/covid19/Empresas en Boyaca/Datos/desempleo2.xlsx")

names(desempleo)
 ## convert to dataframe

#Crear fechas
meses <- c("01-01-07","02-01-07","03-01-07","04-01-07","05-01-07","06-01-07","07-01-07","08-01-07","09-01-07","10-01-07","11-01-07","12-01-07",
           "01-01-08","02-01-08","03-01-08","04-01-08","05-01-08","06-01-08","07-01-08","08-01-08","09-01-08","10-01-08","11-01-08","12-01-08",
           "01-01-09","02-01-09","03-01-09","04-01-09","05-01-09","06-01-09","07-01-09","08-01-09","09-01-09","10-01-09","11-01-09","12-01-09",
           "01-01-10","02-01-10","03-01-10","04-01-10","05-01-10","06-01-10","07-01-10","08-01-10","09-01-10","10-01-10","11-01-10","12-01-10",
           "01-01-11","02-01-11","03-01-11","04-01-11","05-01-11","06-01-11","07-01-11","08-01-11","09-01-11","10-01-11","11-01-11","12-01-11",
           "01-01-12","02-01-12","03-01-12","04-01-12","05-01-12","06-01-12","07-01-12","08-01-12","09-01-12","10-01-12","11-01-12","12-01-12",
           "01-01-13","02-01-13","03-01-13","04-01-13","05-01-13","06-01-13","07-01-13","08-01-13","09-01-13","10-01-13","11-01-13","12-01-13",
           "01-01-14","02-01-14","03-01-14","04-01-14","05-01-14","06-01-14","07-01-14","08-01-14","09-01-14","10-01-14","11-01-14","12-01-14",
           "01-01-15","02-01-15","03-01-15","04-01-15","05-01-15","06-01-15","07-01-15","08-01-15","09-01-15","10-01-15","11-01-15","12-01-15",
           "01-01-16","02-01-16","03-01-16","04-01-16","05-01-16","06-01-16","07-01-16","08-01-16","09-01-16","10-01-16","11-01-16","12-01-16",
           "01-01-17","02-01-17","03-01-17","04-01-17","05-01-17","06-01-17","07-01-17","08-01-17","09-01-17","10-01-17","11-01-17","12-01-17",
           "01-01-18","02-01-18","03-01-18","04-01-18","05-01-18","06-01-18","07-01-18","08-01-18","09-01-18","10-01-18","11-01-18","12-01-18",
           "01-01-19","02-01-19","03-01-19","04-01-19","05-01-19","06-01-19","07-01-19","08-01-19","09-01-19","10-01-19")

meses <- data.frame(meses)

desempleo$date <- as.Date(meses$meses, "%m-%d-%y")
desempleo$year <- year(desempleo$date)

desempleo<- desempleo %>% gather("Tunja","23 ciudades y áreas metropolitanas",key="aforo",value="valor",-year)



ggplot(data = desempleo, aes(x=date, y=valor, fill=aforo))+
  geom_line(aes(color = aforo), size = 1) + xlab("") + ylab("Tasa de desempleo") +
  scale_color_manual(values = c("#FC4E07","#E7B800")) +
  theme_minimal() +theme(legend.position='bottom') + stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))#https://www.w3schools.com/sql/func_mysql_date_format.asp   para cambiar formatos "%Y"












p <- ggplot(MySample, aes(date, y, fill = year)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ year, scales = "free") +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_fill_gradient(breaks=unique(MySample$year))

fig <- ggplotly(p)





+ scale_x_date()

#labs(title = "Ocupación informal (2007-2019)",
 #      caption = "Fuente: GEIH Mercado laboral - empleo informal y seguridad social  \n Elaborado por: Juan Pablo Cely") +

desempleodep <- read_excel("~/Documents/Investigacion/Alto ricaurte/Empresas en Boyaca/Datos/desempleodep.xlsx")
%df <- desempleodep[desempleodep$Departamento %in% c("Tunja","23 ciudades y áreas metropolitanas"), ]
#no es necesario activar
ggplot(desempleodep, aes(x = date, y = Desempleo)) + 
  geom_line(aes(color = Departamento), size = 1) + xlab("") + ylab("Tasa de desempleo") +
  scale_color_manual(values = c("#00AFBB","#E7B800")) +
  theme_minimal() +theme(legend.position='bottom') + stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) 

