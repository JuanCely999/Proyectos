#############################################################################
###Analisis de mercado: Proyecto VISOTA#####################################
######################Autor: Juan Pablo Cely#################################
###############################24-06-2020####################################
# Library
library(fmsb)
library(readxl)

#------------------Diagrama de estacionalidad-----------------------------#
#------------------metro cuadrado-----------------------------------------#
meses <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/meses.xlsx")


# Create data: note in High school for several students
set.seed(99)
meses<-data.frame(meses)


rownames(meses) <- paste(2015:2019)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(6720000,12) , rep(1910000,12) , meses)

# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(5, "BuPu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.3)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.1, y=-0.6, legend = rownames(data[-c(1,2),]), bty = "n", pch= 20, col=colors_in , text.col = "black", cex=0.8, pt.cex=3)








#--------------------Unidades de viviendas--------------------------#

rm(list=ls())
meses <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/meses2.xlsx")


# Create data: note in High school for several students
set.seed(99)
meses<-data.frame(meses)


rownames(meses) <- paste(2015:2019)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(73000,12) , rep(21100,12) , meses)

# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(5, "BuPu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.3)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.1, y=-0.6, legend = rownames(data[-c(1,2),]), bty = "n", pch= 20, col=colors_in , text.col = "black", cex=0.8, pt.cex=3)




# Viviendas en colombia 
#https://www.datanalytics.com/libro_r/la-funcion-melt-y-datos-en-formato-largo.html
#Importancia de melt
colunidad <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/colunidad.xlsx")
cegma.long = melt(colunidad)

colunidad2<-melt(colunidad, id.vars = c("Año"))#id se mantiene quieta
colunidad2 = rename(colunidad2, c(`Número de viviendas`=value, Viviendas=variable))
ggplot(data = colunidad2)+ 
  geom_point(mapping = aes(x = Año, y = `Número de viviendas`, color = Viviendas))+ 
  geom_line(mapping = aes(x = Año, y = `Número de viviendas`, color = Viviendas))+
  theme(legend.position='bottom')+scale_x_continuous(breaks=2006:2019)+ theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position='bottom')


# Viviendas en Boyaca 
#https://www.datanalytics.com/libro_r/la-funcion-melt-y-datos-en-formato-largo.html
#Importancia de melt
boyunidad <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/boyunidad.xlsx")
cegma.long = melt(boyunidad)

boyunidad2<-melt(boyunidad, id.vars = c("Año"))
boyunidad2 = rename(boyunidad2, c(`Número de viviendas`=value, Viviendas=variable))
ggplot(data = boyunidad2)+ 
  geom_point(mapping = aes(x = Año, y = `Número de viviendas`, color = Viviendas))+ 
  geom_line(mapping = aes(x = Año, y = `Número de viviendas`, color = Viviendas))+
  theme(legend.position='bottom')+scale_x_continuous(breaks=2006:2019)+ theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position='bottom')






#-------------------------Proyección de ladrillos Colombia-------------------------#
ladri <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/pronladri.xlsx")
ggplot(data = ladri)+ 
  geom_point(mapping = aes(x = Año, y = `Número de ladrillos`))+
  geom_line(mapping = aes(x = Año, y = `Número de ladrillos`,))+
  scale_color_manual(values = c("#E7B800")) +
  geom_vline( xintercept=2019)+scale_x_continuous(breaks=2016:2023)+ theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position='bottom')

#-------------------------Proyección de ladrillos Boyaca-------------------------#
ladriboy <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/boypronladri.xlsx")
ggplot(data = ladriboy)+ 
  geom_point(mapping = aes(x = Año, y = `Número de ladrillos`))+
  geom_line(mapping = aes(x = Año, y = `Número de ladrillos`,))+
  scale_color_manual(values = c("#E7B800")) +
  geom_vline( xintercept=2019)+scale_x_continuous(breaks=2016:2023)+ theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position='bottom')
#SOMBRAS PARA DETERMINAR LOS CICLOS 
#https://bioinfo.iric.ca/ggplot2-101-easy-visualization-for-easier-analysis/
#https://ugoproto.github.io/ugo_r_doc/plot_snippets_ggplot2/   #Muchas graficas



######################################ANIMACIONES A NIVEL NACIONAL VIS
viviegif<- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/viviegif.xlsx")

colombia <-  st_read(dsn = "./15_BOYACA/", layer = "depto")


colom19 <- colombia %>% 
  left_join(viviegif)

colom19 <- cbind(colom19, st_coordinates(st_centroid(colom19$geometry)))

i <- ggplot(colom19)+
  geom_sf(aes(fill=VIS))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = VIS),
             colour = 'red', alpha = .5) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000, 22000, 24000, 26000, 28000, 30000))+ 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(Año)+labs(size = 'VIS') +
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Evolución de la Vivienda de interés social en Colombia",
       caption = "Fuente: ELIC") + theme_void()


animate(i, renderer = gifski_renderer("VIS.gif"))
######################################ANIMACIONES A NIVEL NACIONAL VIS
f <- ggplot(colom19)+
  geom_sf(aes(fill=NoVIS))+  xlab("") + ylab("") + 
  geom_point(aes(x = X, y = Y, size = NoVIS),
             colour = 'red', alpha = .5) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000, 22000, 24000, 26000, 28000, 30000))+ 
  scale_fill_gradientn(colours = viridis(256, option = "D"))+
  transition_manual(Año)+labs(size = 'NoVIS') +
  labs(subtitle = "Año: {current_frame}")+
  labs(title = "Evolución de la Vivienda diferente a la de interés social en Colombia",
       caption = "Fuente: ELIC") + theme_void()


animate(f, renderer = gifski_renderer("No VIS.gif"))












#-------------------------Pronostico en Arima que no funciona-------------#
library(forecast)
library(ggplot2)
library(readxl)
vivienda2 <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/vivienda.xlsx")

boy <- 
  ts(vivienda2$vivienda,   #La series es mensual, 12 puntos de datos.   
     start=2007,frequency = 1)

# autoplot of a ts object
autoplot(boy) + geom_forecast(h=4)
plot(prediccion)


View(fc)
fc <- forecast(boy)
autoplot(fc)
