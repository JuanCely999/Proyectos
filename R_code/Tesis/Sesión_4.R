#########################################################################################
#############                   BANCO DE LA REP?BLICA                  ##################
#############            Chap. 15 Introductory Econometrics             ##################
#########################################################################################
cat("\f")
rm(list = ls())
library(stats)
library(openxlsx)
library(car)
library(AER)
library(dynlm)
library(gmm)
library(haven)


dir <- "~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Banrep GMM"

#####
# Ejemplo No. 1 Estimaci?n HAC 

data("USMacroG", package = "AER")
consump1 <- dynlm(consumption ~ dpi + L(dpi),
                  data = USMacroG)
#Test para heteroced?sticidad
bptest(consump1)
#Test para autocorrelaci?n
dwtest(consump1)
Box.test(residuals(consump1), type = "Ljung-Box")
#Pesos de kernel para HAC basada en kernel 
curve(kweights(x, kernel = "Quadratic", normalize = TRUE),
      from = 0, to = 3.2, xlab = "x", ylab = "k(x)")
curve(kweights(x, kernel = "Bartlett", normalize = TRUE),
      from = 0, to = 3.2, col = 2, add = TRUE)
curve(kweights(x, kernel = "Parzen", normalize = TRUE),
      from = 0, to = 3.2, col = 3, add = TRUE)
curve(kweights(x, kernel = "Tukey", normalize = TRUE),
      from = 0, to = 3.2, col = 4, add = TRUE)
curve(kweights(x, kernel = "Truncated", normalize = TRUE),
      from = 0, to = 3.2, col = 5, add = TRUE)
#Estimaci?n robusta y comparaci?n
var<-rbind(SE = sqrt(diag(vcov(consump1))),
           QS = sqrt(diag(kernHAC(consump1))),
           NW = sqrt(diag(NeweyWest(consump1))))
#####
# Ejercicio No. 1 HAC usando otro tipo de Kernel

data(Investment)

#####
# Ejemplo No. 2 Variables Instrumentales para educaci?n 

#Leer base de datos
data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),1)
attach(data)

# Muestra restringida a observaciones existentes de Wage 
datos <- subset(data, !is.na(wage))  

# Regresi?n a partir de OLS
reg.ols <- lm( log(wage) ~ educ, data= datos)
summary(reg.ols)

# ?Est?n correlacionadas educ y fatheduc?
summary(lm(educ ~ fatheduc, data = datos))

# Regresi?n a partir de Variables Instrumentales 
reg.iv <- ivreg(log(wage) ~ educ | fatheduc, data= datos) 
summary(reg.iv)

# Regresi?n por medio de la funci?n GMM
regmm <- gmm(log(wage) ~ educ, fatheduc, type="twoStep",
             wmatrix = "optimal")
coeftest(regmm)

#####
#  Ejemplo No. 4 Variables Instrumentales -M?ltiple- 

#Leer base de datos
data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),2)
attach(data)

# Comprobando la relevancia de nuestra Variable Instrumental (nearc4): 
redf <-lm(educ ~ nearc4+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
           reg663+reg664+reg665+reg666+reg667+reg668+reg669, data= data)
summary(redf)

# Estimaci?n a trav?s de OLS PAG 527

ols <-lm(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
          reg663+reg664+reg665+reg666+reg667+reg668+reg669, data=data)

# Estimaci?n a trav?s de Variables Instrumentales 

iv <-ivreg(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669 
           | nearc4+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
           , data=data)
coeftest(iv)
# Regresi?n con GMM
reg.gmm <- gmm(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+
                 reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669 
               ,~nearc4+exper+I(exper^2)+black+smsa+south+smsa66+
                 reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
              ,vcov="optimal")
coef(summary(reg.gmm))


#####
#   Ejemplo No. 5 M?nimos Cuadrados en dos Etapas

data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),1)

# Muestra restringida a observaciones existentes de Wage 
subdata <- subset(data, !is.na(wage))
attach(subdata)

# Primer Etapa: Estimaci?n de la VI para educ
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
summary(etp1)
# Haciendo los coeficientes de las posibles instrum. 0
etp11 <- lm(educ~ exper + I(exper^2), data= subdata)
summary(etp11)
# Estad?stico F
anova(etp1, etp11)

# Segunda Etapa: Estimaci?n del modelo de inter?s
etp2 <-lm(log(wage)~ fitted(etp1)+exper+I(exper^2), data= subdata)
summary(etp2)

# Estimaci?n autom?tica 
MC2E<- ivreg(log(wage)~educ+exper+I(exper^2) 
                | motheduc+fatheduc+exper+I(exper^2) , data= subdata)
coef(summary(MC2E))

#Sin especificar todas las ex?genas
MC2E2 <- ivreg(log(wage)~educ+exper+I(exper^2) |.-educ+fatheduc+motheduc,data=subdata)
coef(summary(MC2E2))

#Con funci?n gmm
gmmMC2E <- gmm(log(wage)~educ+exper+I(exper^2) 
    ,~motheduc+fatheduc+exper+I(exper^2),vcov="iid")
coef(summary(gmmMC2E))

#Diagn?stico
#Relevancia del instrumento
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
Ftest <- waldtest(etp1,.~.-fatheduc-motheduc)

#Test de Hausman-Hu
#Primera etapa
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
#Se a?aden residuales
res <-lm(log(wage)~ educ+exper+I(exper^2)+etp1$residuals, data= subdata)
#Test de wald
HausWutest <- waldtest(res,.~.-etp1$residuals)
HausWutest

#Test de Sargan
#Regresi?n con residuos de MC2E
Sargan_reg <- lm(MC2E$residuals~exper+I(exper^2)+fatheduc+motheduc,data=subdata)
sum <- summary(Sargan_reg)
#Test
Sargan_test <- sum$r.squared*nrow(subdata)
#Estad?stico de prueba
Sargan_test
#P valor
(1-pchisq(Sargan_test,1))  





##################################################################
#######################################NKPCPOR DEPARTAMENTOS TRIMESTRES
##################################################################
######PRUEBA NKPC para Antioquia en trimestres  inicial(2009)
nacional <- read_dta("Actualizacion/Bases de datos con filtros/TrimestreCorrido.dta")
nacional <- read_dta("Actualizacion/Bases de datos con filtros/TrimestreInicial.dta")

names(nacional)
#primero los costos marginales
nacional<- data.frame(nacional[,c(54,28)])
nacional<-na.omit(nacional)

nacional <- 
  ts(nacional,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 4)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(nacional[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(nacional[,2],-3))

cm<-cbind(nacional[,1])
for (i in 1:7) cm<-cbind(cm,lag(nacional[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(nacional[,2],3))

#ipc2<-lag(nacional$anticm,3)


antio<-cbind(ipca3,ipca2,cm,nacional[,1:2])
#antio<-na.omit(antio)
antio<-na.omit(antio)

colnames(antio)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")
#antio<-na.omit(antio)



gmmt<-gmm(ipc~ipct_1+ipct1+cmt_1,
          ~ipct1+ipct_1+cmt_1,
          ~cmt_1,ipct_1+ipct1,data =antio, vcov="HAC")

coeftest(gmmt)
coef(summary(gmmt))
summary(gmmt)
#https://tulengua.es/numeros-texto/default.aspx
#https://www.youtube.com/watch?v=BQveFKP5oWw&list=LL4UZhLuowFzG66hy1j0Ytvw&index=6&t=0s









######PRUEBA NKPC para Antioquia en meses (2010)

nacional <- read_dta("Actualizacion/Bases de datos con filtros/MesesCorrido.dta")
nacional <- read_dta("Actualizacion/Bases de datos con filtros/Mesesnormales.dta")

names(nacional)
#primero los costos marginales
nacional<- data.frame(nacional[,c(60,31)])
nacional<-na.omit(nacional)

nacional <- 
  ts(nacional,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)

detach("package:dplyr", unload = TRUE)

ipcat<-cbind(nacional[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(nacional[,2],-3))

cm<-cbind(nacional[,1])
for (i in 1:7) cm<-cbind(cm,lag(nacional[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(nacional[,2],3))

#ipc2<-lag(nacional$anticm,3)


antio<-cbind(ipca3,ipca2,cm,nacional[,1:2])
#antio<-na.omit(antio)
antio<-na.omit(antio)

colnames(antio)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")
#antio<-na.omit(antio)



gmmt<-gmm(ipc~ipct_1+ipct1+cmt_1,
          ~ipct1+ipct_1+cmt_1,
          ~cmt_1,ipct_1+ipct1,data =antio, vcov="HAC")

coeftest(gmmt)
coef(summary(gmmt))
summary(gmmt)
#https://tulengua.es/numeros-texto/default.aspx
#https://www.youtube.com/watch?v=BQveFKP5oWw&list=LL4UZhLuowFzG66hy1j0Ytvw&index=6&t=0s































#----------------------------------------------
#----------------------------------------------



######PRUEBA NKPC para Antioquia en meses Reemplazar por 3 y 12
nacional <- read_dta("~/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/Mesesnormales.dta")
names(nacional)



#Antioquia
nacional<- data.frame(nacional[,c(4,29)])



nacional <- 
  ts(nacional,   #La series es mensual, 12 puntos de datos.   
     start=2010,frequency = 12)



detach("package:dplyr", unload = TRUE)

ipcat<-cbind(nacional[,2])
for (i in 1:7) ipca3<-cbind(ipcat,lag(nacional[,2],-3))


cm<-cbind(nacional[,1])
for (i in 1:7) cm<-cbind(cm,lag(nacional[,1],-3))

library(dplyr)
for (i in 1:7) ipca2<-cbind(ipcat,lead(nacional[,2],3))

#ipc2<-lag(nacional$anticm,3)


antio<-cbind(ipca3,ipca2,cm,nacional[,1:2])
#antio<-na.omit(antio)
antio<-na.omit(antio)

colnames(antio)<-c("ipc","ipct_1","ipc2","ipct1","cm","cmt_1","orgpi","orgcm","ipcf","ipct_1f","ipc2f","ipct1f","cmf","cmt_1f")
#antio<-na.omit(antio)



gmmt<-gmm(ipc~ipct_1+ipct1+cmt_1,
          ~ipct1+ipct_1+cmt_1,
          ~cmt_1,ipct_1+ipct1,data =antio, vcov="HAC")

coeftest(gmmt)
coef(summary(gmmt))
summary(gmmt)

ols <-lm(ipc~ipct_1+ipct1+cmt_1, data=antio)
coeftest(ols)

#https://tulengua.es/numeros-texto/default.aspx
#https://www.youtube.com/watch?v=BQveFKP5oWw&list=LL4UZhLuowFzG66hy1j0Ytvw&index=6&t=0s