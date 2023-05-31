#https://rpubs.com/Joaquin_AR/226291#:~:text=La%20regresi%C3%B3n%20lineal%20m%C3%BAltiple%20permite,2%2C%20X3%E2%80%A6).
##############################################################################
###Modelo con las ventas de la NLB
######################Autor: Juan Pablo Cely#################################
###############################15-03-2023####################################
library(car)
library(corrplot)
library(lmtest)
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)
library(gridExtra)# Para organizar múltiples gráficos juntos
library(tidyverse)
library(ggversa)
#library(GGally)
#install.packages("GGally")
setwd("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/")
dat<- read_excel(paste("correlacion.xlsx",sep=""),4)
#dat2<- data.frame(dat[,c(2:34)]) 
#dat22 <- as.data.frame(dat2)

round(cor(x = dat, method = "pearson"), 3)
names(dat)
modelo <- lm(ventas2 ~ inflacion12+ventas1+Estrato1+Estrato2+     
               Estrato3+Estrato4+Estrato11+Estrato22+Estrato33+Estrato44+totalinicial1+
               totalinicial2+cabecera1+cabecera2+emc1+emc2+emmpro21+emmpro22+     
               emmven11+emmven12+ems11+ems21+ems31+ems12+ems22+        
               +ems32+icc+iec+ice+td, data = dat )
summary(modelo)

modelo1 <- lm(ventas2 ~ inflacion1+inflacion12+ventas1+Estrato1+Estrato2+     
               Estrato3+Estrato4+Estrato11+Estrato22+Estrato33+Estrato44+totalinicial1+
               totalinicial2+cabecera1+cabecera2+emc1+emc2+emmpro21+emmpro22+     
               emmven11+emmven12+ems11+ems21+ems31+ems12+ems22+        
               +ems32+icc+iec+ice+td, data = dat )

summary(modelo1)

modelo1 <- lm(ventas2 ~ inflacion12+Estrato22+Estrato11+cabecera1+emc1+emc2+ems21+       
                +ems32+td, data = dat )


modelo1 <- lm(ventas2 ~ inflacion12+Estrato1+Estrato2+     
Estrato3+Estrato4+totalinicial1+
totalinicial2+cabecera1+cabecera2+emmpro22+emmven12+ems11+ems21+
ems31+ems12+ems22+ems32+iec+ice+td, data = dat )



#opcion 1
modelo1 <- lm(ventas2 ~ inflacion12+Estrato11+Estrato22+Estrato33+
                cabecera2+ems12+ems22+        
                ems32+icc+iec+ice+td, data = dat )
#2
modelo1 <- lm(ventas2 ~ inflacion12+Estrato11+Estrato33+
                cabecera2+ems12+        
                ems32+icc+iec+td, data = dat )

#3
modelo1 <- lm(ventas2 ~ inflacion12+Estrato11+Estrato33+
            ems12+        
                ems32+icc+iec+td, data = dat )

### Elegido opcional
modelo1 <- lm(ventas2 ~ inflacion12+Estrato11+
                ems12+        
                ems32+icc+iec+td, data = dat )


### Elegido ideal
modelo1 <- lm(ventas2 ~ inflacion12+Estrato11+
                +iec+td, data = dat )

summary(modelo1) 
step(object = modelo1, direction = "both", trace = 1)


confint(lm(formula = ventas2 ~ inflacion12+Estrato11+
             ems12+        
             ems32+icc+iec+td, data = dat ))

plot1 <- ggplot(data = dat, aes(inflacion12, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = dat, aes(Estrato11, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = dat, aes(ems12, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = dat, aes(ems32, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = dat, aes(icc, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot6 <- ggplot(data = dat, aes(iec, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot7 <- ggplot(data = dat, aes(td, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)


qqnorm(modelo1$residuals)
qqline(modelo1$residuals)

shapiro.test(modelo1$residuals)

ggplot(data = dat, aes(modelo1$fitted.values, modelo1$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

bptest(modelo1)


corrplot(cor(dplyr::select(dat, inflacion12,Estrato11,
                            iec,td)),
         method = "number", tl.col = "black")


vif(modelo1)

dwt(modelo1, alternative = "two.sided")



dat$studentized_residual <- rstudent(modelo1)
ggplot(data = dat, aes(x = predict(modelo1), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


which(abs(dat$studentized_residual) > 3)

summary(influence.measures(modelo1))

influencePlot(modelo1)
