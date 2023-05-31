##############################################################################
#                  Tesis:Proxy para calcular las ventas de la NLB              #
#                            Autor: Juan Pablo Cely                         #
###############################21-03-2023####################################

library(corrplot)
library(tempdisagg)
library(dplyr)
#help(tempdisagg)
library(readxl)
library(foreign)
library(readxl)
library(ggplot2)  
library(readxl)
library(tidyr)
library(fpp2)
library(dplyr)
library(haven)
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
library(reshape2)

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
library(reshape2)
library(tsbox)
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","final2022")])

setwd("~/Documents/Investigacion/Licorera_Boyaca/Bases_de_datos/Pronostico/")
atla2<- read_excel(paste("NLB_datos.xlsx",sep=""),1)
related_var<- read_excel(paste("NLB_datos.xlsx",sep=""),2)
related_var22<- read_excel(paste("NLB_datos.xlsx",sep=""),3)


atla22 <- 
  ts(atla2$anual,   #La series es mensual, 12 puntos de datos.   
     start=2014,frequency = 1)

related_var2 <- 
  ts(related_var$mes,   #La series es mensual, 12 puntos de datos.   
     start=2014,frequency = 12)

#anti3 <- td(atla22 ~ related_var2, conversion = "average",to = "monthly", method = "chow-lin-maxlog")
anti3 <- td(atla22 ~ related_var2, conversion = "sum",to = "monthly", method = "chow-lin-maxlog")
antimes<-data.frame(anti=predict(anti3))

#write.csv(antimes, file = "antimes3.csv")#suma
#write.csv(antimes, file = "antimes2.csv")#promedio
#write.csv(antimes, file = "antimes.csv")#promedio


#anti4 <- td(atla22 ~ related_var2, conversion = "average",to = "monthly", method = "chow-lin-maxlog")


#anti4 <- td(atla22 ~ related_var2, conversion = "first",to = "monthly", method = "chow-lin-maxlog")


#antimes4<-data.frame(anti=predict(anti4))

#write.csv(antimes4, file = "antimes4.csv")
#serie <- ts(antimes4, frequency= ,start=)


#####
####################Uso del siguiente link
#https://rstudio-pubs-static.s3.amazonaws.com/855973_05b0301e30a54be7afd13073caef3a99.html

# Rango de filas a borrar (por ejemplo, filas 3 a 5)
rango_filas_borrar <- 1:72

# Borrar el rango de filas especificado
antimes <- antimes[-rango_filas_borrar, ]
antimes<-data.frame(antimes)

data_serie <- ts(antimes$anti, frequency=12, start=2020)
#data_serie <- ts(antimes$anti, frequency=12, start=2014)
#z<-data_frame(data_serie)






autoplot(data_serie)+
  labs(title = "Serie de tiempo",       
       x = "Tiempo",
       y = "Valor",
       colour = "#00a0dc")+
  theme_bw() 



# Descomposición de la serie de tiempo. Se almacena en el objeto fit
fit <- decompose(data_serie, type='additive')
#fit <- decompose(data_serie, type='multiplicative')

# Para graficar esta descomposición volvemos a emplear la funcion autoplot, pero con el objeto fit
autoplot(fit)+
  labs(title = "Descomposición de la serie de tiempo",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+
  theme_bw()


#Grafico con tendencia
autoplot(data_serie, series="Serie tiempo") + 
  autolayer(trendcycle(fit), series="Tendencia") +
  labs(title = "Serie de tiempo",      
       x = "Tiempo",
       y = "Valor"
  ) + 
  theme_bw()



#Grafico de estacionalidad
ggseasonplot(data_serie)


#Metodo pronostico
#Metodos simples
# elaborando el método
m1 <- snaive(data_serie, h=60)

# graficando el pronóstico
a1<-autoplot(m1)+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  xlab("Año") + ylab("Ventas")+ theme_minimal()+ 
  labs(title = "Método simple")


print(m1)
#write.csv(m1, file = "m1.csv")

# verificando el ajuste del método
autoplot(m1)+autolayer(fitted(m1), series="Ajuste")


# verificando los residuales
checkresiduals(m1)



#METODO DE REGRESION
# elaborando la regresion
regresion <- tslm(data_serie ~ trend + season)

# elaborando el pronostico
m2 <- forecast(regresion, h=60)

# graficando el pronóstico
a2<-autoplot(m2)+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  xlab("Año") + ylab("Ventas")+ theme_minimal()+ 
  labs(title = "Método de regresión")

# verificando el ajuste del método
autoplot(m2)+autolayer(fitted(m2), series="Ajuste")

# verificando los residuales
checkresiduals(m2)


#Método holt winters
m3 <- hw(data_serie, h=60, seasonal = 'multiplicative')


# graficando el pronóstico
a3<-autoplot(m3)+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  xlab("Año") + ylab("Ventas")+ theme_minimal()+ 
  labs(title = "Modelo Holt Winters")


# verificando el ajuste del método
autoplot(m3)+autolayer(fitted(m3), series="Ajuste")

# verificando los residuales
checkresiduals(m3)


#Modelo Arima
# elaborando el modelo ARIMA
modelo_arima <- auto.arima(data_serie)

# elaborando el pronostico
m4 <- forecast(modelo_arima, h=60)

# graficando el pronóstico
#Parte importante para mirar los resultados coeficientes y demas
a4<-autoplot(m4)+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  xlab("Año") + ylab("Ventas")+ theme_minimal()+ 
  labs(title = "Modelo Arima")


# verificando el ajuste del método
autoplot(m4)+autolayer(fitted(m4), series="Ajuste")

# verificando los residuales
checkresiduals(m4)


#########Red neuronal
# elaborando el modelo de red neuronal
neural_network <- nnetar(data_serie)

# elaborando el pronostico
m5 <- forecast(neural_network, h=60)

# graficando el pronóstico
#autoplot(m5)+ scale_x_date(labels = date_format("%Y"), breaks = "1 year")

a5<-autoplot(m5)+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  xlab("Año") + ylab("Ventas")+ theme_minimal()+ 
  labs(title = "Modelo de red neuronal")
# verificando el ajuste del método
autoplot(m5)+autolayer(fitted(m5), series="Ajuste")


# verificando los residuales
checkresiduals(m5)

#IMPORTANTE
grid.arrange(a1,a2,a3,a4,a5)





#Revisar interprestaciones y adicion del modelo

real <- c(50877)
data_real <- ts(real, frequency=12,start=2023)


# modelo en base a métodos simples
accuracy(m1,data_real)

# modelo en base a regresion lineal
accuracy(m2,data_real)


# modelo en base a holt winters
accuracy(m3,data_real)
m3[["mean"]]

# modelo en base a ARIMA
accuracy(m4,data_real)
m4[["mean"]]

# modelo en base a red neuronal
accuracy(m5,data_real)
m5[["mean"]]




######################################
######################################
setwd("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/")
correlacion2 <- read_excel(paste("correlacion.xlsx",sep=""),1)
correlacion <- read_excel(paste("correlacion.xlsx",sep=""),3)
c<- data.frame(correlacion[,c(3,5,11,19,21,23,27:29)]) 
correlacion22<- data.frame(correlacion2[,c(2:29)]) 
names(c)

bb<-cor(c$ventas2, c$inflacion12, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")


a<-ggplot(data = c) +
  geom_point(mapping = aes(x = inflacion12, y =ventas2))+  xlab("Inflación") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = inflacion12, y =ventas2)) + annotate("text", x = 0.06, y = 0, label = xx)+ theme_bw()

bb<-cor(c$ventas2, c$Estrato22, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

b<-ggplot(data = c) +
  geom_point(mapping = aes(x = Estrato22, y =ventas2))+  xlab("Estrato 2") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = Estrato22, y =ventas2)) + annotate("text", x = 0.06, y = 0, label = xx)+ theme_bw()


bb<-cor(c$ventas2, c$emc2, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

cc<-ggplot(data = c) +
  geom_point(mapping = aes(x = emc2, y =ventas2))+  xlab("Ventas por bebidas") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = emc2, y =ventas2)) + annotate("text", x = 0.06, y = 0, label = xx)+ theme_bw()


bb<-cor(c$ventas2, c$emmpro22, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

d<-ggplot(data = c) +
  geom_point(mapping = aes(x = emmpro22, y =ventas2))+  xlab("Producción real de bebidas") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = emmpro22, y =ventas2)) + annotate("text", x = 0, y = 0, label = xx)+ theme_bw()


bb<-cor(c$ventas2, c$emmven12, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

e<-ggplot(data = c) +
  geom_point(mapping = aes(x = emmven12, y =ventas2))+  xlab("Venta real de bebidas") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = emmven12, y =ventas2)) + annotate("text", x = 0, y = 0, label = xx)+ theme_bw()


bb<-cor(c$ventas2, c$ems12, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

f<-ggplot(data = c) +
  geom_point(mapping = aes(x = ems12, y =ventas2))+  xlab("Ingreso por servicios") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = ems12, y =ventas2)) + annotate("text", x = 0.06, y = 0, label = xx)+ theme_bw()


bb<-cor(c$ventas2, c$ems22, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

g<-ggplot(data = c) +
  geom_point(mapping = aes(x = ems22, y =ventas2))+  xlab("Ingreso por venta") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = ems22, y =ventas2)) + annotate("text", x = 0.06, y = 0, label = xx)+ theme_bw()

bb<-cor(c$ventas2, c$ems32, use='complete.obs')
bbb<-bb[1]
xx <- paste0(round(bbb*100,2), "%")

h<-ggplot(data = c) +
  geom_point(mapping = aes(x = ems32, y =ventas2))+  xlab("Ingreso total") + ylab("Ventas") +
  geom_smooth(mapping = aes(x = ems32, y =ventas2)) + annotate("text", x = 0.0, y = 0, label = xx)+ theme_bw()



grid.arrange(a,b,cc,d,e,f,g,h)



M <- cor(correlacion22)
#corrplot(M, method = "ellipse")
#corrplot.mixed(M)

#SEGUIR ESTE COMANDO
corrplot(M, method="number", type="upper")

#cor(c$ventas2, c$inflacion12, use='complete.obs')













# Establecer configuración de la figura
# par(mfrow = c(1, 1)) # 3 filas, 3 columnas
# 
# plot(c$ventas2, c$inflacion12,type = "m", pch=20, col='blue',
#      xlab='Inflación', las=1,
#      ylab='Ventas')
# text(1, 2, "Ejemplo de texto") 
# 
# plot(c$ventas2, c$Estrato22, pch=20, col='blue',
#      xlab='Estrato 2', las=1,
#      ylab='Ventas')
# text(4, 80, "Ejemplo de texto") 
# 
# 
# abline(lm(c$ventas2 ~ c$Estrato22), col = "red", lwd = 3)
# text(paste("Correlación:", round(cor(c$ventas2, c$Estrato22), 2)), x = 10, y = 95)

# Establecer la ubicación de cada gráfico en la figura
#layout(matrix(c(1,2,3,4,5,6,7), nrow = 4, ncol = 3))



# , method='pearson')
# , method='kendall')
# , method='spearman')

# a<-with(c, plot(x=ventas2, y=ems12, pch=20, col='blue',
#                  xlab='Área del apartamento', las=1,
#                  ylab='Precio del apartamento (millones COP)'))
# 
# with(c, plot(x=ventas2, y=ems12, pch=20, col='blue',
#                 xlab='Área del apartamento', las=1,
#                 ylab='Precio del apartamento (millones COP)'))
# 
# 
# # Creamos el gráfico
# plot(c$ventas2, c$ems12, pch = 19, col = "lightblue")
# # 
# # Línea de regresión
# abline(lm(c$ventas2 ~ c$ems12), col = "red", lwd = 3)
# 
# # Correlación de Pearson
# text(paste("Correlación:", round(cor(c$ventas2, c$ems12,), 2)), x = 10, y = 95)
# 
# # Crear los gráficos y establecer su tamaño
# plot(c$ventas2, c$ems12, type = "l", main = "Gráfico 1", ylim = c(0, 100), xlim = c(0, 10))
# plot(c$ventas2, c$ems12, type = "p", col = "blue", main = "Gráfico 2", ylim = c(0, 1000), xlim = c(0, 10))
# plot(c$ventas2, c$ems12, type = "b", col = "red", main = "Gráfico 3", ylim = c(0, 1100), xlim = c(0, 10))

# Establecer la ubicación de cada gráfico en la figura
# layout(matrix(c(1,2,3,4,5,6,7), nrow = 1, ncol = 3))




