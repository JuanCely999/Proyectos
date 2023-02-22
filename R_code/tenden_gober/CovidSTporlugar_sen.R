rm(list=ls())
library(lubridate)
library(dplyr)
library(readr)
library(rtweet)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(hrbrthemes)
library(tm)
library(devtools)
library(tidyverse)
library(sentimentr)
library(gridExtra)

api_key<- "epb66XxlmRXoJMV9EVuKHrqLH"
api_secret<- "hjzbWFUCUtbdxFZmbqmDKNmL7ZN10u84GBErynalxxbtQymJbK"
access_token<-"1012856937292226562-fCIjLw2xF0XYwk0c8GxgqVbrmOQzFw"
access_token_secret<-"wR1gNompUCNIxbLmhu1E6p6ufHZRMJMwGcEftEUOWMpOg"
twitter_app         <- "TOURisgover"


# Accedemos a Twitter a trav?s de los datos del token
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

######################################################
######################################################
#Guardar Base de datos
setwd("~/Documents/Investigacion//Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal")


######################################################
#             Tweets en Colombia                    #
######################################################
######################################################
######################################################
#2101turcol = 21 de enero turismo en colombia
colombia1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja
colombia2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja

colombia11<- data.frame(colombia1[,c(1:16,35:68,73:84)])
write.csv(colombia11, file = "c191105turcol.csv")
colombia22<- data.frame(colombia2[,c(1:16,35:68,73:84)])
write.csv(colombia22, file = "c191105gobcol.csv")


######################################################
#             Tweets en Latinoamerica                    #
######################################################
######################################################
######################################################

sudamerica1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  " -15.60079,-56.06096,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de cuiaba
sudamerica2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  " -15.60079,-56.06096,2000km", time = "2005-01-01 2019-10-30")

sudamerica11<- data.frame(sudamerica1[,c(1:16,35:68,73:84)])
write.csv(sudamerica11, file = "c191105tursud.csv")
sudamerica22<- data.frame(sudamerica2[,c(1:16,35:68,73:84)])
write.csv(sudamerica22, file = "c191105gobsud.csv")

######################################################
######################################################
#             Tweets en America                   #
######################################################
######################################################
######################################################

norte1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "45.01627,-93.26566,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de Minneapolis, USA
norte2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "45.01627,-93.26566,2000km", time = "2005-01-01 2019-10-30")

norte11<- data.frame(norte1[,c(1:16,35:68,73:84)])
write.csv(norte11, file = "c191105turnor.csv")
norte22<- data.frame(norte2[,c(1:16,35:68,73:84)])
write.csv(norte22, file = "c191105gobnor.csv")

######################################################
######################################################
#             Tweets en Asia                 #
######################################################
######################################################
######################################################

asia1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "30.93019,90.32588,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de tibet, China
asia2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "30.93019,90.32588,2000km", time = "2005-01-01 2019-10-30")

asia11<- data.frame(asia1[,c(1:16,35:68,73:84)])
write.csv(asia11, file = "c191105turasia.csv")
asia22<- data.frame(asia2[,c(1:16,35:68,73:84)])
write.csv(asia22, file = "c191105gobasia.csv")



######################################################
######################################################
#             Tweets en Africa                   #
######################################################
######################################################
######################################################

africa1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-18.98468,32.67164,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de mutare, Zimbabwe
africa2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-18.98468,32.67164,2000km", time = "2005-01-01 2019-10-30")

africa11<- data.frame(africa1[,c(1:16,35:68,73:84)])
write.csv(africa11, file = "c191105turafr.csv")
africa22<- data.frame(africa2[,c(1:16,35:68,73:84)])
write.csv(africa22, file = "c191105gobafr.csv")




######################################################
######################################################
######################################################
#             Tweets en Europa                  #
######################################################
######################################################
######################################################

europa1<- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "47.81027,12.98202,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de salzburg, Austria
europa2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "47.81027,12.98202,2000km", time = "2005-01-01 2019-10-30")

europa11<- data.frame(europa1[,c(1:16,35:68,73:84)])
write.csv(europa11, file = "c191105tureur.csv")
europa22<- data.frame(europa2[,c(1:16,35:68,73:84)])
write.csv(europa22, file = "c191105gobeur.csv")

######################################################
######################################################
#             Tweets en Oceania               #
######################################################
######################################################
######################################################

oceania1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-16.26248,133.41772,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de Daly Waters, Australia
oceania2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-16.26248,133.41772,2000km", time = "2005-01-01 2019-10-30")

oceania11<- data.frame(oceania1[,c(1:16,35:68,73:84)])
write.csv(oceania11, file = "c191105turoce.csv")
oceania22<- data.frame(oceania2[,c(1:16,35:68,73:84)])
write.csv(oceania22, file = "c191105goboce.csv")

rm(list=ls())

#######TENER EN CUENTA#########GRAFICA DE VIOLIN
################################COLOMBIA#####################
colombia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turcol.csv")
colombia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobcol.csv")

colGen=rbind(colombia1,colombia2) 

colGen$text <- as.character(colGen$text)
sentiment_scores1 <- sentiment_by(colGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores1)
b<-a[1]
Lugar <- rep("Colombia" , b)
final1 <- sentiment_scores1$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final <- data.frame(final1,Lugar)
final <- filter(final, final1!=0)
names(final)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final <- filter(final, Sentimiento<ati)
final <- filter(final, Sentimiento>ati2)
summary(final$Sentimiento)

################################Norteamerica#####################
norte1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turnor.csv")
norte2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobnor.csv")

norGen=rbind(norte1,norte2) 

norGen$text <- as.character(norGen$text)
sentiment_scores2 <- sentiment_by(norGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores2)
b<-a[1]
Lugar <- rep("Norteam?rica" , b)
final1 <- sentiment_scores2$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final2 <- data.frame(final1,Lugar)
final2 <- filter(final2, final1!=0)
names(final2)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final2$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final2 <- filter(final2, Sentimiento<ati)
final2 <- filter(final2, Sentimiento>ati2)
summary(final2$Sentimiento)

################################Suramerica#####################
sud1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tursud.csv")
sud2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobsud.csv")

sudGen=rbind(sud1,sud2) 

sudGen$text <- as.character(sudGen$text)
sentiment_scores3 <- sentiment_by(sudGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores3)
b<-a[1]
Lugar <- rep("Suram?rica" , b)
final1 <- sentiment_scores3$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final3 <- data.frame(final1,Lugar)
final3 <- filter(final3, final1!=0)
names(final3)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final3$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final3 <- filter(final3, Sentimiento<ati)
final3 <- filter(final3, Sentimiento>ati2)
summary(final3$Sentimiento)

################################Europa#####################
eur1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tureur.csv")
eur2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobeur.csv")

eurGen=rbind(eur1,eur2) 

eurGen$text <- as.character(eurGen$text)
sentiment_scores4 <- sentiment_by(eurGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores4)
b<-a[1]
Lugar <- rep("Europa" , b)
final1 <- sentiment_scores4$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final4 <- data.frame(final1,Lugar)
final4 <- filter(final4, final1!=0)
names(final4)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final4$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final4 <- filter(final4, Sentimiento<ati)
final4 <- filter(final4, Sentimiento>ati2)
summary(final4$Sentimiento)


################################Sur de asia#####################
asia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turasia.csv")
asia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobasia.csv")

asiaGen=rbind(asia1,asia2) 

asiaGen$text <- as.character(asiaGen$text)
sentiment_scores5 <- sentiment_by(asiaGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores5)
b<-a[1]
Lugar <- rep("Sur de Asia" , b)
final1 <- sentiment_scores5$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final5 <- data.frame(final1,Lugar)
final5 <- filter(final5, final1!=0)
names(final5)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final5$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final5 <- filter(final5, Sentimiento<ati)
final5 <- filter(final5, Sentimiento>ati2)
summary(final5$Sentimiento)

################################sur de africa#####################
afr1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turafr.csv")
afr2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobafr.csv")

afrGen=rbind(afr1,afr2) 

afrGen$text <- as.character(afrGen$text)
sentiment_scores6 <- sentiment_by(afrGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores6)
b<-a[1]
Lugar <- rep("Sur de Africa" , b)
final1 <- sentiment_scores6$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final6 <- data.frame(final1,Lugar)
final6 <- filter(final6, final1!=0)
names(final6)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final6$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final6 <- filter(final6, Sentimiento<ati)
final6 <- filter(final6, Sentimiento>ati2)
summary(final6$Sentimiento)

################################oceania#####################
oce1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turoce.csv")
oce2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105goboce.csv")

oceGen=rbind(oce1,oce2) 

oceGen$text <- as.character(oceGen$text)
sentiment_scores7 <- sentiment_by(oceGen$text)
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores7)
b<-a[1]
Lugar <- rep("Oceania" , b)
final1 <- sentiment_scores7$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final7 <- data.frame(final1,Lugar)
final7 <- filter(final7, final1!=0)
names(final7)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final7$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final7 <- filter(final7, Sentimiento<ati)
final7 <- filter(final7, Sentimiento>ati2)
summary(final7$Sentimiento)
setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")


total=rbind(final,final2, final3, final4, final5, final6, final7)


png("gencovid.png",width = 448,height = 289)
ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal()+geom_hline( yintercept=0)+  scale_fill_grey() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
dev.off()
dim(final)
dim(final2)
dim(final3)
dim(final4)
dim(final5)
dim(final6)
dim(final7)


summary(final$Sentimiento)
summary(final2$Sentimiento)
summary(final3$Sentimiento)
summary(final4$Sentimiento)
summary(final5$Sentimiento)
summary(final6$Sentimiento)
summary(final7$Sentimiento)
summarise(final, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final2, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final3, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final4, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final5, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final6, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final7, var = var(Sentimiento), sd= sd(Sentimiento))
quantile(final$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                




rm(list=ls())


#######################################################
#######################################################
#######################################################
######################TURISMO##########################
############COLOMBIA##########
colGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turcol.csv")
colGen$text <- as.character(colGen$text)
sentiment_scores1 <- sentiment_by(colGen$text)

#Consruccion base de datos
colomtur<- cbind(colGen,sentiment_scores1)
colomtur<- data.frame(colomtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(colomtur, file = "colturcovid.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores1)
b<-a[1]
Lugar <- rep("Colombia" , b)
final1 <- sentiment_scores1$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final <- data.frame(final1,Lugar)
final <- filter(final, final1!=0)
names(final)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final <- filter(final, Sentimiento<ati)
final <- filter(final, Sentimiento>ati2)
summary(final$Sentimiento)



################################Norteamerica#####################
norGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turnor.csv")
norGen$text <- as.character(norGen$text)
sentiment_scores2 <- sentiment_by(norGen$text)

# Construccion base de datos
nortur<- cbind(norGen,sentiment_scores2)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "norturcovid.csv")


#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores2)
b<-a[1]
Lugar <- rep("Norteamérica" , b)
final1 <- sentiment_scores2$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final2 <- data.frame(final1,Lugar)
final2 <- filter(final2, final1!=0)
names(final2)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final2$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final2 <- filter(final2, Sentimiento<ati)
final2 <- filter(final2, Sentimiento>ati2)
summary(final2$Sentimiento)

################################Suramerica#####################
sudGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tursud.csv")
sudGen$text <- as.character(sudGen$text)
sentiment_scores3 <- sentiment_by(sudGen$text)
# Construccion base de datos
surtur<- cbind(sudGen,sentiment_scores3)
stur<- data.frame(surtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(stur, file = "surturcovid.csv")

#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores3)
b<-a[1]
Lugar <- rep("Suramérica" , b)
final1 <- sentiment_scores3$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final3 <- data.frame(final1,Lugar)
final3 <- filter(final3, final1!=0)
names(final3)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final3$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final3 <- filter(final3, Sentimiento<ati)
final3 <- filter(final3, Sentimiento>ati2)
summary(final3$Sentimiento)

################################Europa#####################
eurGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tureur.csv")
eurGen$text <- as.character(eurGen$text)
sentiment_scores4 <- sentiment_by(eurGen$text)
etur<- cbind(eurGen,sentiment_scores4)
e<- data.frame(etur[,c(2:5,7,28,47:53,57:67)])
#write.csv(e, file = "eurturcovid.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores4)
b<-a[1]
Lugar <- rep("Europa" , b)
final1 <- sentiment_scores4$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final4 <- data.frame(final1,Lugar)
final4 <- filter(final4, final1!=0)
names(final4)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final4$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final4 <- filter(final4, Sentimiento<ati)
final4 <- filter(final4, Sentimiento>ati2)
summary(final4$Sentimiento)


################################Sur de asia#####################
asiaGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turasia.csv")
asiaGen$text <- as.character(asiaGen$text)
sentiment_scores5 <- sentiment_by(asiaGen$text)

asitur<- cbind(asiaGen,sentiment_scores5)
a<- data.frame(asitur[,c(2:5,7,28,47:53,57:67)])
#write.csv(a, file = "asiaturcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores5)
b<-a[1]
Lugar <- rep("Sur de Asia" , b)
final1 <- sentiment_scores5$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final5 <- data.frame(final1,Lugar)
final5 <- filter(final5, final1!=0)
names(final5)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final5$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final5 <- filter(final5, Sentimiento<ati)
final5 <- filter(final5, Sentimiento>ati2)
summary(final5$Sentimiento)

################################sur de africa#####################
afrGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turafr.csv")
afrGen$text <- as.character(afrGen$text)
sentiment_scores6 <- sentiment_by(afrGen$text)

afrtur<- cbind(afrGen,sentiment_scores6)
aftur<- data.frame(afrtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(aftur, file = "afrturcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores6)
b<-a[1]
Lugar <- rep("Sur de Africa" , b)
final1 <- sentiment_scores6$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final6 <- data.frame(final1,Lugar)
final6 <- filter(final6, final1!=0)
names(final6)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final6$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final6 <- filter(final6, Sentimiento<ati)
final6 <- filter(final6, Sentimiento>ati2)
summary(final6$Sentimiento)

################################oceania#####################
oceGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turoce.csv")
oceGen$text <- as.character(oceGen$text)
sentiment_scores7 <- sentiment_by(oceGen$text)

nortur<- cbind(oceGen,sentiment_scores7)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "oceturcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores7)
b<-a[1]
Lugar <- rep("Oceania" , b)
final1 <- sentiment_scores7$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final7 <- data.frame(final1,Lugar)
final7 <- filter(final7, final1!=0)
names(final7)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final7$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final7 <- filter(final7, Sentimiento<ati)
final7 <- filter(final7, Sentimiento>ati2)
summary(final7$Sentimiento)


total=rbind(final,final2, final3, final4, final5, final6, final7)


png("turcovid.png",width = 448,height = 289)
ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal()+geom_hline( yintercept=0)+  scale_fill_grey() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
dev.off()
dim(final)
dim(final2)
dim(final3)
dim(final4)
dim(final5)
dim(final)
dim(final2)
dim(final3)
dim(final4)
dim(final5)
dim(final6)
dim(final7)


summary(final$Sentimiento)
summary(final2$Sentimiento)
summary(final3$Sentimiento)
summary(final4$Sentimiento)
summary(final5$Sentimiento)
summary(final6$Sentimiento)
summary(final7$Sentimiento)
summarise(final, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final2, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final3, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final4, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final5, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final6, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final7, var = var(Sentimiento), sd= sd(Sentimiento))
quantile(final$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                



rm(list=ls())
setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")
#######################################################
#######################################################
#######################################################
######################GOBERNANZA##########################
############COLOMBIA##########
colGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobcol.csv")
colGen$text <- as.character(colGen$text)
sentiment_scores1 <- sentiment_by(colGen$text)

colotur<- cbind(colGen,sentiment_scores1)
cotur<- data.frame(colotur[,c(2:5,7,28,47:53,57:67)])
#write.csv(cotur, file = "colgobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores1)
b<-a[1]
Lugar <- rep("Colombia" , b)
final1 <- sentiment_scores1$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final <- data.frame(final1,Lugar)
final <- filter(final, final1!=0)
names(final)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final <- filter(final, Sentimiento<ati)
final <- filter(final, Sentimiento>ati2)
summary(final$Sentimiento)

################################Norteamerica#####################
norGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobnor.csv")
norGen$text <- as.character(norGen$text)
sentiment_scores2 <- sentiment_by(norGen$text)

nortur<- cbind(norGen,sentiment_scores2)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "norgobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores2)
b<-a[1]
Lugar <- rep("Norteamérica" , b)
final1 <- sentiment_scores2$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final2 <- data.frame(final1,Lugar)
final2 <- filter(final2, final1!=0)
names(final2)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final2$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final2 <- filter(final2, Sentimiento<ati)
final2 <- filter(final2, Sentimiento>ati2)
summary(final2$Sentimiento)

################################Suramerica#####################
sudGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobsud.csv")
sudGen$text <- as.character(sudGen$text)
sentiment_scores3 <- sentiment_by(sudGen$text)

stur<- cbind(sudGen,sentiment_scores3)
stur<- data.frame(stur[,c(2:5,7,28,47:53,57:67)])
#write.csv(stur, file = "surgobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores3)
b<-a[1]
Lugar <- rep("Suramérica" , b)
final1 <- sentiment_scores3$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final3 <- data.frame(final1,Lugar)
final3 <- filter(final3, final1!=0)
names(final3)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final3$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final3 <- filter(final3, Sentimiento<ati)
final3 <- filter(final3, Sentimiento>ati2)
summary(final3$Sentimiento)

################################Europa#####################
eurGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobeur.csv")
eurGen$text <- as.character(eurGen$text)
sentiment_scores4 <- sentiment_by(eurGen$text)

etur<- cbind(eurGen,sentiment_scores4)
etur<- data.frame(etur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "eurgobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores4)
b<-a[1]
Lugar <- rep("Europa" , b)
final1 <- sentiment_scores4$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final4 <- data.frame(final1,Lugar)
final4 <- filter(final4, final1!=0)
names(final4)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final4$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final4 <- filter(final4, Sentimiento<ati)
final4 <- filter(final4, Sentimiento>ati2)
summary(final4$Sentimiento)


################################Sur de asia#####################
asiaGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobasia.csv")
asiaGen$text <- as.character(asiaGen$text)
sentiment_scores5 <- sentiment_by(asiaGen$text)

asitur<- cbind(asiaGen,sentiment_scores5)
astur<- data.frame(asitur[,c(2:5,7,28,47:53,57:67)])
#write.csv(astur, file = "asiagobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores5)
b<-a[1]
Lugar <- rep("Sur de Asia" , b)
final1 <- sentiment_scores5$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final5 <- data.frame(final1,Lugar)
final5 <- filter(final5, final1!=0)
names(final5)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final5$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final5 <- filter(final5, Sentimiento<ati)
final5 <- filter(final5, Sentimiento>ati2)
summary(final5$Sentimiento)

################################sur de africa#####################
afrGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobafr.csv")
afrGen$text <- as.character(afrGen$text)
sentiment_scores6 <- sentiment_by(afrGen$text)

afrtur<- cbind(afrGen,sentiment_scores6)
aftur<- data.frame(afrtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(aftur, file = "afrgobcovid.csv")

#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores6)
b<-a[1]
Lugar <- rep("Sur de Africa" , b)
final1 <- sentiment_scores6$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final6 <- data.frame(final1,Lugar)
final6 <- filter(final6, final1!=0)
names(final6)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final6$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final6 <- filter(final6, Sentimiento<ati)
final6 <- filter(final6, Sentimiento>ati2)
summary(final6$Sentimiento)

################################oceania#####################
oceGen<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105goboce.csv")
oceGen$text <- as.character(oceGen$text)
sentiment_scores7 <- sentiment_by(oceGen$text)

ocetur<- cbind(oceGen,sentiment_scores7)
octur<- data.frame(ocetur[,c(2:5,7,28,47:53,57:67)])
#write.csv(octur, file = "ocegobcovid.csv")
#write.csv(sentiment_scores, file = "colsent.csv")
#view(sentiment_scores)
######################################################
a<-dim(sentiment_scores7)
b<-a[1]
Lugar <- rep("Oceania" , b)
final1 <- sentiment_scores7$ave_sentiment
#plot(sentiment_scores, ordered = TRUE)
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final7 <- data.frame(final1,Lugar)
final7 <- filter(final7, final1!=0)
names(final7)= c("Sentimiento", "Lugar")
#png("col2.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
# xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
r<-summary(final7$Sentimiento) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
final7 <- filter(final7, Sentimiento<ati)
final7 <- filter(final7, Sentimiento>ati2)
summary(final7$Sentimiento)


total=rbind(final,final2, final3, final4, final5, final6, final7)


png("gobcovid.png",width = 448,height = 289)
ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal()+geom_hline( yintercept=0)+  scale_fill_grey() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
dev.off()
dim(final)
dim(final2)
dim(final3)
dim(final4)
dim(final5)
dim(final6)
dim(final7)


summary(final$Sentimiento)
summary(final2$Sentimiento)
summary(final3$Sentimiento)
summary(final4$Sentimiento)
summary(final5$Sentimiento)
summary(final6$Sentimiento)
summary(final7$Sentimiento)
summarise(final, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final2, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final3, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final4, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final5, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final6, var = var(Sentimiento), sd= sd(Sentimiento))
summarise(final7, var = var(Sentimiento), sd= sd(Sentimiento))
quantile(final$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final2$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final3$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final4$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final5$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final6$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(final7$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                


rm(list=ls())


###############################################################
##################Mineria de opinion############################
###############################################################
###############################################################

setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")
#####Paso a seguir: borrar valores de 0
#https://ggplot2.tidyverse.org/reference/geom_bar.html

coltur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turcol.csv")
colgob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobcol.csv")

nortur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turnor.csv")
norgob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobnor.csv")

surtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tursud.csv")
surgob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobsud.csv")

eurtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105tureur.csv")
eurgob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobeur.csv")

asiatur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turasia.csv")
asiagob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobasia.csv")

afrtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turafr.csv")
afrgob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105gobafr.csv")

ocetur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105turoce.csv")
ocegob<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/c191105goboce.csv")



###########Colombia######
coltur$text <- as.character(coltur$text)
sentiment_scores1 <- emotion(coltur$text)
sentiment_scores1 <- filter(sentiment_scores1, emotion_count!=0)
a<-dim(sentiment_scores1)
b<-a[1]
Lugar <- rep("Colombia1" , b)
final1 <- sentiment_scores1$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final9 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
colgob$text <- as.character(colgob$text)
sentiment_scores11 <- emotion(colgob$text)
sentiment_scores11 <- filter(sentiment_scores11, emotion_count!=0)
a<-dim(sentiment_scores11)
b<-a[1]
Lugar <- rep("Colombia2" , b)
final1 <- sentiment_scores11$emotion_type
final99 <- data.frame(final1,Lugar)

###########Norteamerica######
nortur$text <- as.character(nortur$text)
sentiment_scores2 <- emotion(nortur$text)
sentiment_scores2 <- filter(sentiment_scores2, emotion_count!=0)
a<-dim(sentiment_scores2)
b<-a[1]
Lugar <- rep("nor" , b)
final1 <- sentiment_scores2$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final2 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
norgob$text <- as.character(norgob$text)
sentiment_scores22 <- emotion(norgob$text)
sentiment_scores22 <- filter(sentiment_scores22, emotion_count!=0)
a<-dim(sentiment_scores22)
b<-a[1]
Lugar <- rep("nor2" , b)
final1 <- sentiment_scores22$emotion_type
final22 <- data.frame(final1,Lugar)
#final4 <- filter(final4, final1!=0)
#total=rbind(final,final4)
#plot(total)

###########Sudamerica######
surtur$text <- as.character(surtur$text)
sentiment_scores3 <- emotion(surtur$text)
sentiment_scores3 <- filter(sentiment_scores3, emotion_count!=0)
a<-dim(sentiment_scores3)
b<-a[1]
Lugar <- rep("sur1" , b)
final1 <- sentiment_scores3$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final3 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
surgob$text <- as.character(surgob$text)
sentiment_scores33 <- emotion(surgob$text)
sentiment_scores33 <- filter(sentiment_scores33, emotion_count!=0)
a<-dim(sentiment_scores33)
b<-a[1]
Lugar <- rep("sur2" , b)
final1 <- sentiment_scores33$emotion_type
final33 <- data.frame(final1,Lugar)

###########Europa######
eurtur$text <- as.character(eurtur$text)
sentiment_scores4 <- emotion(eurtur$text)
sentiment_scores4 <- filter(sentiment_scores4, emotion_count!=0)
a<-dim(sentiment_scores4)
b<-a[1]
Lugar <- rep("eur1" , b)
final1 <- sentiment_scores4$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final4 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
eurgob$text <- as.character(eurgob$text)
sentiment_scores44 <- emotion(eurgob$text)
sentiment_scores44 <- filter(sentiment_scores44, emotion_count!=0)
a<-dim(sentiment_scores44)
b<-a[1]
Lugar <- rep("eur2" , b)
final1 <- sentiment_scores44$emotion_type
final44 <- data.frame(final1,Lugar)

###########Asia######
asiatur$text <- as.character(asiatur$text)
sentiment_scores5 <- emotion(asiatur$text)
sentiment_scores5 <- filter(sentiment_scores5, emotion_count!=0)
a<-dim(sentiment_scores5)
b<-a[1]
Lugar <- rep("asia1" , b)
final1 <- sentiment_scores5$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final5 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
asiagob$text <- as.character(asiagob$text)
sentiment_scores55 <- emotion(asiagob$text)
sentiment_scores55 <- filter(sentiment_scores55, emotion_count!=0)
a<-dim(sentiment_scores55)
b<-a[1]
Lugar <- rep("asia2" , b)
final1 <- sentiment_scores55$emotion_type
final55 <- data.frame(final1,Lugar)
###########africa######
afrtur$text <- as.character(afrtur$text)
sentiment_scores6 <- emotion(afrtur$text)
sentiment_scores6 <- filter(sentiment_scores6, emotion_count!=0)
a<-dim(sentiment_scores6)
b<-a[1]
Lugar <- rep("afr1" , b)
final1 <- sentiment_scores6$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final6 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
afrgob$text <- as.character(afrgob$text)
sentiment_scores66 <- emotion(afrgob$text)
sentiment_scores66 <- filter(sentiment_scores66, emotion_count!=0)
a<-dim(sentiment_scores66)
b<-a[1]
Lugar <- rep("afr2" , b)
final1 <- sentiment_scores66$emotion_type
final66 <- data.frame(final1,Lugar)

###########oceania######
ocetur$text <- as.character(ocetur$text)
sentiment_scores7 <- emotion(ocetur$text)
sentiment_scores7 <- filter(sentiment_scores7, emotion_count!=0)
a<-dim(sentiment_scores7)
b<-a[1]
Lugar <- rep("oce1" , b)
final1 <- sentiment_scores7$emotion_type
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final7 <- data.frame(final1,Lugar)
#coltur=rbind(final,final4)
ocegob$text <- as.character(ocegob$text)
sentiment_scores77 <- emotion(ocegob$text)
sentiment_scores77 <- filter(sentiment_scores77, emotion_count!=0)
a<-dim(sentiment_scores77)
b<-a[1]
Lugar <- rep("oce2" , b)
final1 <- sentiment_scores77$emotion_type
final77 <- data.frame(final1,Lugar)
#g <- ggplot(total, aes(Lugar))
#g + geom_bar(aes(fill = final1))
#tabulate(final2$final1)
com9<-data.frame(table(final9$final1))#relevante para tesis
com99<-data.frame(table(final99$final1))
com2<-data.frame(table(final2$final1))
com22<-data.frame(table(final22$final1))
com3<-data.frame(table(final3$final1))
com33<-data.frame(table(final33$final1))
com4<-data.frame(table(final4$final1))
com44<-data.frame(table(final44$final1))
com5<-data.frame(table(final5$final1))
com55<-data.frame(table(final55$final1))
com6<-data.frame(table(final6$final1))
com66<-data.frame(table(final66$final1))
com7<-data.frame(table(final7$final1))
com77<-data.frame(table(final77$final1))



union <- rbind(com9,com99,com2,com22,com3,com33,com4,com44,com5,com55,com6,com66,com7,com77)
pais <- c(rep("Colombia-Turismo" , 16) , rep("Colombia-Gobernanza" ,16),
          rep("Norteamérica-Turismo" , 16) , rep("Norteamérica-Gobernanza" ,16),
          rep("Suramérica-Turismo" , 16) , rep("Suramérica-Gobernanza" ,16),
          rep("Europa-Turismo" , 16) , rep("Europa-Gobernanza" ,16),
          rep("Sur de Asia-Turismo" , 16) , rep("Sur de Asia-Gobernanza" ,16),
          rep("Sur de Africa-Turismo" , 16) , rep("Sur de Africa-Gobernanza" ,16),
          rep("Oceania-Turismo" , 16) , rep("Oceania-Gobernanza" ,16))
data <- data.frame(pais,union)
data <- filter(data, Freq!=0)
data <- filter(data, Var1!="anger_negated")
data <- filter(data, Var1!="anticipation_negated")
data <- filter(data, Var1!="disgust_negated")
data <- filter(data, Var1!="fear_negated")
data <- filter(data, Var1!="joy_negated")
data <- filter(data, Var1!="surprise_negated")
data <- filter(data, Var1!="trust_negated")
data <- filter(data, Var1!="sadness_negated")

#Recopilar en porcentajes importante: Recoge paleta de colores
#https://rpubs.com/Rortizdu/140190
data = rename(data, c(pais="Paises", Var1="Emociones", Freq="Porcentaje"))
names (data)
png("sentimientocovid.png",width = 448,height = 289)
ggplot(data, aes(fill=Emociones, y=Porcentaje, x=Paises, fill=Color)) + 
  geom_bar(position="fill", stat="identity") +   scale_fill_grey() +  coord_flip()
dev.off()



######################INCIDENCIA POR SEGUIDORES###############################
##############################################################################

rm(list=ls())





coltur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/colturcovid.csv")


colneu<- filter(coltur, ave_sentiment==0 )
colneg<- filter(coltur, ave_sentiment <0 )
colpos<- filter(coltur, ave_sentiment >0 )

dim(colpos)
dim(colneg)
dim(colneu)


#sur
surtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/surturcovid.csv")


surneu<- filter(surtur, ave_sentiment==0 )
surneg<- filter(surtur, ave_sentiment <0 )
surpos<- filter(surtur, ave_sentiment >0 )

dim(surpos)
dim(surneg)
dim(surneu)

#nor
nortur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/norturcovid.csv")


norneu<- filter(nortur, ave_sentiment==0 )
norneg<- filter(nortur, ave_sentiment <0 )
norpos<- filter(nortur, ave_sentiment >0 )

dim(norpos)
dim(norneg)
dim(norneu)


#asi
asitur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/asiaturcovid.csv")


asineu<- filter(asitur, ave_sentiment==0 )
asineg<- filter(asitur, ave_sentiment <0 )
asipos<- filter(asitur, ave_sentiment >0 )

dim(asipos)
dim(asineg)
dim(asineu)

#afr
afrtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/afrturcovid.csv")


afrneu<- filter(afrtur, ave_sentiment==0 )
afrneg<- filter(afrtur, ave_sentiment <0 )
afrpos<- filter(afrtur, ave_sentiment >0 )

dim(afrpos)
dim(afrneg)
dim(afrneu)

#eur
eurtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/eurturcovid.csv")


eurneu<- filter(eurtur, ave_sentiment==0 )
eurneg<- filter(eurtur, ave_sentiment <0 )
eurpos<- filter(eurtur, ave_sentiment >0 )

dim(eurpos)
dim(eurneg)
dim(eurneu)

#oce
ocetur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/oceturcovid.csv")


oceneu<- filter(ocetur, ave_sentiment==0 )
oceneg<- filter(ocetur, ave_sentiment <0 )
ocepos<- filter(ocetur, ave_sentiment >0 )

dim(ocepos)
dim(oceneg)
dim(oceneu)



##############################################
##############################################
##############################################
######################Gobernanza#############
##############################################
#Col
colgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/colgobcovid.csv")


colneu<- filter(colgober, ave_sentiment==0 )
colneg<- filter(colgober, ave_sentiment <0 )
colpos<- filter(colgober, ave_sentiment >0 )

dim(colpos)
dim(colneg)
dim(colneu)


#sur
surgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/surgobcovid.csv")


surneu<- filter(surgober, ave_sentiment==0 )
surneg<- filter(surgober, ave_sentiment <0 )
surpos<- filter(surgober, ave_sentiment >0 )

dim(surpos)
dim(surneg)
dim(surneu)

#nor
norgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/norgobcovid.csv")


norneu<- filter(norgober, ave_sentiment==0 )
norneg<- filter(norgober, ave_sentiment <0 )
norpos<- filter(norgober, ave_sentiment >0 )

dim(norpos)
dim(norneg)
dim(norneu)


#asi
asigober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/asiagobcovid.csv")


asineu<- filter(asigober, ave_sentiment==0 )
asineg<- filter(asigober, ave_sentiment <0 )
asipos<- filter(asigober, ave_sentiment >0 )

dim(asipos)
dim(asineg)
dim(asineu)

#afr
afrgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/afrgobcovid.csv")


afrneu<- filter(afrgober, ave_sentiment==0 )
afrneg<- filter(afrgober, ave_sentiment <0 )
afrpos<- filter(afrgober, ave_sentiment >0 )

dim(afrpos)
dim(afrneg)
dim(afrneu)

#eur
eurgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/eurgobcovid.csv")


eurneu<- filter(eurgober, ave_sentiment==0 )
eurneg<- filter(eurgober, ave_sentiment <0 )
eurpos<- filter(eurgober, ave_sentiment >0 )

dim(eurpos)
dim(eurneg)
dim(eurneu)

#oce
ocegober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/ocegobcovid.csv")


oceneu<- filter(ocegober, ave_sentiment==0 )
oceneg<- filter(ocegober, ave_sentiment <0 )
ocepos<- filter(ocegober, ave_sentiment >0 )

dim(ocepos)
dim(oceneg)
dim(oceneu)






##############################################
##############################################
##############################################
######################Creacion boxplot#########
##############################################
#######################Turismo###############

turboxplot<-rbind(coltur, surtur, nortur, asitur, afrtur, eurtur)
turboxplot<-turboxplot[!duplicated(turboxplot$followers_count),]
#turboxplot<-filter(turboxplot, followers_count <10000 )
turboxplot<-filter(turboxplot, followers_count <100000 )


turpos<- filter(turboxplot, ave_sentiment >0 )
turneg<- filter(turboxplot, ave_sentiment <0 )
turneu<- filter(turboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(turpos)
b<-a[1]
sent <- rep("Positivo" , b)
postur1 <- turpos$followers_count
postur2 <- data.frame(sent,postur1)
names(postur2)= c("Sentimiento", "Seguidores")

r<-summary(postur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
postur3 <- filter(postur2, Seguidores<ati)
postur3 <- filter(postur3, Seguidores>ati2)


#NEGATIVO

a<-dim(turneg)
b<-a[1]
sent <- rep("Negativo" , b)
negtur1 <- turneg$followers_count
negtur2 <- data.frame(sent,negtur1)
names(negtur2)= c("Sentimiento", "Seguidores")

r<-summary(negtur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
negtur3 <- filter(negtur2, Seguidores<ati)
negtur3 <- filter(negtur3, Seguidores>ati2)


#NEUTRO

a<-dim(turneu)
b<-a[1]
sent <- rep("Neutro" , b)
neutur1 <- turneu$followers_count
neutur2 <- data.frame(sent,neutur1)
names(neutur2)= c("Sentimiento", "Seguidores")


r<-summary(neutur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neutur3 <- filter(neutur2, Seguidores<ati)
neutur3 <- filter(neutur3, Seguidores>ati2)



finaltur<-rbind(postur3,negtur3,neutur3)

e<-ggplot(data = finaltur, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) +
  labs(subtitle = "Turismo 0-100.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  

quantile(postur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neutur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(negtur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                








turboxplot<-rbind(coltur, surtur, nortur, asitur, afrtur, eurtur)
turboxplot<-turboxplot[!duplicated(turboxplot$followers_count),]
#turboxplot<-filter(turboxplot, followers_count <10000 )
turboxplot<-filter(turboxplot, followers_count <1000000 )
turboxplot<-filter(turboxplot, followers_count >100000 )


turpos<- filter(turboxplot, ave_sentiment >0 )
turneg<- filter(turboxplot, ave_sentiment <0 )
turneu<- filter(turboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(turpos)
b<-a[1]
sent <- rep("Positivo" , b)
postur1 <- turpos$followers_count
postur2 <- data.frame(sent,postur1)
names(postur2)= c("Sentimiento", "Seguidores")

r<-summary(postur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
postur3 <- filter(postur2, Seguidores<ati)
postur3 <- filter(postur3, Seguidores>ati2)


#NEGATIVO

a<-dim(turneg)
b<-a[1]
sent <- rep("Negativo" , b)
negtur1 <- turneg$followers_count
negtur2 <- data.frame(sent,negtur1)
names(negtur2)= c("Sentimiento", "Seguidores")

r<-summary(negtur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
negtur3 <- filter(negtur2, Seguidores<ati)
negtur3 <- filter(negtur3, Seguidores>ati2)


#NEUTRO

a<-dim(turneu)
b<-a[1]
sent <- rep("Neutro" , b)
neutur1 <- turneu$followers_count
neutur2 <- data.frame(sent,neutur1)
names(neutur2)= c("Sentimiento", "Seguidores")


r<-summary(neutur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neutur3 <- filter(neutur2, Seguidores<ati)
neutur3 <- filter(neutur3, Seguidores>ati2)



finaltur<-rbind(postur3,negtur3,neutur3)

f<-ggplot(data = finaltur, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) +
  labs(subtitle = "Turismo 100.000-1.000.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  

quantile(postur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neutur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(negtur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                








turboxplot<-rbind(coltur, surtur, nortur, asitur, afrtur, eurtur)
turboxplot<-turboxplot[!duplicated(turboxplot$followers_count),]
#turboxplot<-filter(turboxplot, followers_count <10000 )
turboxplot<-filter(turboxplot, followers_count <5000000 )
turboxplot<-filter(turboxplot, followers_count >1000000 )


turpos<- filter(turboxplot, ave_sentiment >0 )
turneg<- filter(turboxplot, ave_sentiment <0 )
turneu<- filter(turboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(turpos)
b<-a[1]
sent <- rep("Positivo" , b)
postur1 <- turpos$followers_count
postur2 <- data.frame(sent,postur1)
names(postur2)= c("Sentimiento", "Seguidores")

r<-summary(postur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
postur3 <- filter(postur2, Seguidores<ati)
postur3 <- filter(postur3, Seguidores>ati2)


#NEGATIVO

a<-dim(turneg)
b<-a[1]
sent <- rep("Negativo" , b)
negtur1 <- turneg$followers_count
negtur2 <- data.frame(sent,negtur1)
names(negtur2)= c("Sentimiento", "Seguidores")

r<-summary(negtur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
negtur3 <- filter(negtur2, Seguidores<ati)
negtur3 <- filter(negtur3, Seguidores>ati2)


#NEUTRO

a<-dim(turneu)
b<-a[1]
sent <- rep("Neutro" , b)
neutur1 <- turneu$followers_count
neutur2 <- data.frame(sent,neutur1)
names(neutur2)= c("Sentimiento", "Seguidores")


r<-summary(neutur2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neutur3 <- filter(neutur2, Seguidores<ati)
neutur3 <- filter(neutur3, Seguidores>ati2)



finaltur<-rbind(postur3,negtur3,neutur3)

i<-ggplot(data = finaltur, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) +
  labs(subtitle = "Turismo >1.000.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  

quantile(postur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neutur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(negtur3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                

















################Gobernanza


goberboxplot<-rbind(colgober, surgober, norgober, asigober, afrgober, eurgober)
goberboxplot<-goberboxplot[!duplicated(goberboxplot$followers_count),]
goberboxplot<-filter(goberboxplot, followers_count <100000 )

goberpos<- filter(goberboxplot, ave_sentiment >0 )
goberneg<- filter(goberboxplot, ave_sentiment <0 )
goberneu<- filter(goberboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(goberpos)
b<-a[1]
sent <- rep("Positivo" , b)
posgober1 <- goberpos$followers_count
posgober2 <- data.frame(sent,posgober1)
names(posgober2)= c("Sentimiento", "Seguidores")

r<-summary(posgober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
posgober3 <- filter(posgober2, Seguidores<ati)
posgober3 <- filter(posgober3, Seguidores>ati2)


#NEGATIVO

a<-dim(goberneg)
b<-a[1]
sent <- rep("Negativo" , b)
neggober1 <- goberneg$followers_count
neggober2 <- data.frame(sent,neggober1)
names(neggober2)= c("Sentimiento", "Seguidores")

r<-summary(neggober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neggober3 <- filter(neggober2, Seguidores<ati)
neggober3 <- filter(neggober3, Seguidores>ati2)


#NEUTRO

a<-dim(goberneu)
b<-a[1]
sent <- rep("Neutro" , b)
neugober1 <- goberneu$followers_count
neugober2 <- data.frame(sent,neugober1)
names(neugober2)= c("Sentimiento", "Seguidores")


r<-summary(neugober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neugober3 <- filter(neugober2, Seguidores<ati)
neugober3 <- filter(neugober3, Seguidores>ati2)



finalgober<-rbind(posgober3,neggober3,neugober3)

g<-ggplot(data = finalgober, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  labs(subtitle = "Gobernanza 0-100.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  

quantile(posgober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neugober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neggober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                








goberboxplot<-rbind(colgober, surgober, norgober, asigober, afrgober, eurgober)
goberboxplot<-goberboxplot[!duplicated(goberboxplot$followers_count),]
goberboxplot<-filter(goberboxplot, followers_count <1000000 )
goberboxplot<-filter(goberboxplot, followers_count >100000 )

goberpos<- filter(goberboxplot, ave_sentiment >0 )
goberneg<- filter(goberboxplot, ave_sentiment <0 )
goberneu<- filter(goberboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(goberpos)
b<-a[1]
sent <- rep("Positivo" , b)
posgober1 <- goberpos$followers_count
posgober2 <- data.frame(sent,posgober1)
names(posgober2)= c("Sentimiento", "Seguidores")

r<-summary(posgober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
posgober3 <- filter(posgober2, Seguidores<ati)
posgober3 <- filter(posgober3, Seguidores>ati2)


#NEGATIVO

a<-dim(goberneg)
b<-a[1]
sent <- rep("Negativo" , b)
neggober1 <- goberneg$followers_count
neggober2 <- data.frame(sent,neggober1)
names(neggober2)= c("Sentimiento", "Seguidores")

r<-summary(neggober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neggober3 <- filter(neggober2, Seguidores<ati)
neggober3 <- filter(neggober3, Seguidores>ati2)


#NEUTRO

a<-dim(goberneu)
b<-a[1]
sent <- rep("Neutro" , b)
neugober1 <- goberneu$followers_count
neugober2 <- data.frame(sent,neugober1)
names(neugober2)= c("Sentimiento", "Seguidores")


r<-summary(neugober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neugober3 <- filter(neugober2, Seguidores<ati)
neugober3 <- filter(neugober3, Seguidores>ati2)



finalgober<-rbind(posgober3,neggober3,neugober3)

h<-ggplot(data = finalgober, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  labs(subtitle = "Gobernanza 100.000-1.000.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  


quantile(posgober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neugober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neggober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                









goberboxplot<-rbind(colgober, surgober, norgober, asigober, afrgober, eurgober)
goberboxplot<-goberboxplot[!duplicated(goberboxplot$followers_count),]
goberboxplot<-filter(goberboxplot, followers_count <5000000 )
goberboxplot<-filter(goberboxplot, followers_count >1000000 )

goberpos<- filter(goberboxplot, ave_sentiment >0 )
goberneg<- filter(goberboxplot, ave_sentiment <0 )
goberneu<- filter(goberboxplot, ave_sentiment==0 )

#POSITIVO

a<-dim(goberpos)
b<-a[1]
sent <- rep("Positivo" , b)
posgober1 <- goberpos$followers_count
posgober2 <- data.frame(sent,posgober1)
names(posgober2)= c("Sentimiento", "Seguidores")

r<-summary(posgober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
posgober3 <- filter(posgober2, Seguidores<ati)
posgober3 <- filter(posgober3, Seguidores>ati2)


#NEGATIVO

a<-dim(goberneg)
b<-a[1]
sent <- rep("Negativo" , b)
neggober1 <- goberneg$followers_count
neggober2 <- data.frame(sent,neggober1)
names(neggober2)= c("Sentimiento", "Seguidores")

r<-summary(neggober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neggober3 <- filter(neggober2, Seguidores<ati)
neggober3 <- filter(neggober3, Seguidores>ati2)


#NEUTRO

a<-dim(goberneu)
b<-a[1]
sent <- rep("Neutro" , b)
neugober1 <- goberneu$followers_count
neugober2 <- data.frame(sent,neugober1)
names(neugober2)= c("Sentimiento", "Seguidores")


r<-summary(neugober2$Seguidores) #eliminar outliers
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 1.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 1.5* IQR 
neugober3 <- filter(neugober2, Seguidores<ati)
neugober3 <- filter(neugober3, Seguidores>ati2)



finalgober<-rbind(posgober3,neggober3,neugober3)

j<-ggplot(data = finalgober, aes(x = Sentimiento, y = Seguidores)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Sentimiento), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  labs(subtitle = "Gobernanza >1.000.000")+ 
  xlab('Sentimiento') + 
  ylab('Seguidores') + guides(fill=FALSE) + theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  


quantile(posgober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neugober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                
quantile(neggober3$Seguidores, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                




grid.arrange(e,g,f,h,i,j)



#quantile(goberboxplot$followers_count, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))          # 10 partes,                                                
#quantile(finalgober$Sentimiento, prob = c(0.01, 0.25, 0.5, 0.75, 0.9))          # 10 partes,                                                






