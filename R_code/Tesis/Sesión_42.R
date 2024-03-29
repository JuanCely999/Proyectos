#########################################################################################
#############                   BANCO DE LA REP�BLICA                  ##################
#############            Chap. 15 Introductory Econometrics             ##################
#########################################################################################

rm(list = ls())
library(stats)
library(openxlsx)
library(car)
library(AER)
library(dynlm)
library(gmm)

dir <- "C://Users/CAEP40/Desktop/Econ2018/Dia 4/sesion4/"

#####
# Ejemplo No. 1 Estimaci�n HAC 

data("USMacroG", package = "AER")
consump1 <- dynlm(consumption ~ dpi + L(dpi),
                  data = USMacroG)
#Test para heteroced�sticidad
bptest(consump1)
#Test para autocorrelaci�n
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
#Estimaci�n robusta y comparaci�n
var<-rbind(SE = sqrt(diag(vcov(consump1))),
           QS = sqrt(diag(kernHAC(consump1))),
           NW = sqrt(diag(NeweyWest(consump1))))
#####
# Ejercicio No. 1 HAC usando otro tipo de Kernel

data(Investment)

#####
# Ejemplo No. 2 Variables Instrumentales para educaci�n 

#Leer base de datos

data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),1)
attach(data)

# Muestra restringida a observaciones existentes de Wage 
datos <- subset(data, !is.na(wage))  

# Regresi�n a partir de OLS
reg.ols <- lm( log(wage) ~ educ, data= datos)
summary(reg.ols)

# �Est�n correlacionadas educ y fatheduc?
summary(lm(educ ~ fatheduc, data = datos))

# Regresi�n a partir de Variables Instrumentales 
reg.iv <- ivreg(log(wage) ~ educ | fatheduc, data= datos) 
summary(reg.iv)

# Regresi�n por medio de la funci�n GMM
regmm <- gmm(log(wage) ~ educ, fatheduc, type="twoStep",
             wmatrix = "optimal")
coeftest(regmm)

#####
#  Ejemplo No. 4 Variables Instrumentales -M�ltiple- 

#Leer base de datos
data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),2)
attach(data)

# Comprobando la relevancia de nuestra Variable Instrumental (nearc4): 
redf <-lm(educ ~ nearc4+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
           reg663+reg664+reg665+reg666+reg667+reg668+reg669, data= data)
summary(redf)

# Estimaci�n a trav�s de OLS

ols <-lm(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
          reg663+reg664+reg665+reg666+reg667+reg668+reg669, data=data)

# Estimaci�n a trav�s de Variables Instrumentales 

iv <-ivreg(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669 
           | nearc4+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
           , data=data)
coeftest(iv)
# Regresi�n con GMM
reg.gmm <- gmm(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+
                 reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669 
               ,~nearc4+exper+I(exper^2)+black+smsa+south+smsa66+
                 reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
              ,vcov="optimal")
coef(summary(reg.gmm))


#####
#   Ejemplo No. 5 M�nimos Cuadrados en dos Etapas

data <- read.xlsx(paste(dir,"/GMM",".xlsx",sep=""),1)

# Muestra restringida a observaciones existentes de Wage 
subdata <- subset(data, !is.na(wage))
attach(subdata)

# Primer Etapa: Estimaci�n de la VI para educ
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
summary(etp1)
# Haciendo los coeficientes de las posibles instrum. 0
etp11 <- lm(educ~ exper + I(exper^2), data= subdata)
summary(etp11)
# Estad�stico F
anova(etp1, etp11)

# Segunda Etapa: Estimaci�n del modelo de inter�s
etp2 <-lm(log(wage)~ fitted(etp1)+exper+I(exper^2), data= subdata)
summary(etp2)

# Estimaci�n autom�tica 
MC2E<- ivreg(log(wage)~educ+exper+I(exper^2) 
                | motheduc+fatheduc+exper+I(exper^2) , data= subdata)
coef(summary(MC2E))

#Sin especificar todas las ex�genas
MC2E2 <- ivreg(log(wage)~educ+exper+I(exper^2) |.-educ+fatheduc+motheduc,data=subdata)
coef(summary(MC2E2))

#Con funci�n gmm
gmmMC2E <- gmm(log(wage)~educ+exper+I(exper^2) 
    ,~motheduc+fatheduc+exper+I(exper^2),vcov="iid")
coef(summary(gmmMC2E))

#Diagn�stico
#Relevancia del instrumento
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
Ftest <- waldtest(etp1,.~.-fatheduc-motheduc)

#Test de Hausman-Hu
#Primera etapa
etp1 <- lm(educ~ exper + I(exper^2) + motheduc + fatheduc, data= subdata)
#Se a�aden residuales
res <-lm(log(wage)~ educ+exper+I(exper^2)+etp1$residuals, data= subdata)
#Test de wald
HausWutest <- waldtest(res,.~.-etp1$residuals)
HausWutest

#Test de Sargan
#Regresi�n con residuos de MC2E
Sargan_reg <- lm(MC2E$residuals~exper+I(exper^2)+fatheduc+motheduc,data=subdata)
sum <- summary(Sargan_reg)
#Test
Sargan_test <- sum$r.squared*nrow(subdata)
#Estad�stico de prueba
Sargan_test
#P valor
(1-pchisq(Sargan_test,1))  
