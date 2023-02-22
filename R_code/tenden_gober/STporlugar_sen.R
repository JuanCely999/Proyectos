##############################################################################
###Proyecto colciencias: Turismo y gobernanza################################
#######################Graficas por emociones y resumen del proyecto#########
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
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
#install.packages("sentiment")
#install.packages("devtools")

#source1 <- c(rep("Julian" , 1614), rep("Maria" , 1614))
#ubertweets <- data.frame(uber_tweets,source1)

#source1 <- c(rep("Maria" , 3236))
#ubertweets2 <- data.frame(ubertweets2,source1)
#tweets_julia <- read_csv("data/tweets_julia.csv")
#tweets_dave <- read_csv("data/tweets_dave.csv")
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
setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")

#####Fecha      13-01-20 11pm     a    21-01-20 11pm la ultima es la fecha en que se realiza el ejercicio
#####Proxima    21-01-20 11pm     a    29-01-20 11pm  
#####Proxima    29-01-20 11pm     a    06-02-20 11pm 
#####Proxima    06-02-20 11pm     a    15-02-20 11pm 
#####Proxima    15-02-20 11pm     a    23-02-20 11pm
######################################################
#             Tweets en Colombia                    #
######################################################
######################################################
######################################################
#2101turcol = 21 de enero turismo en colombia
colombia1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja
#ts_plot(colombia1, by = "mins")
#ejercicios de ensayo

#rstats_net <- network_data(colombia1, "retweet,mention,reply")
#rstats_net
#attr(rstats_net, "idsn")
#if (requireNamespace("igraph", quietly = TRUE)) {
 # rstats_net <- network_graph(colombia1)
  #plot(rstats_net)
#}









colombia2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja

colombia11<- data.frame(colombia1[,c(1:16,35:68,73:84)])
write.csv(colombia11, file = "2302turcol.csv")
colombia22<- data.frame(colombia2[,c(1:16,35:68,73:84)])
write.csv(colombia22, file = "2302gobcol.csv")


######################################################
#             Tweets en Latinoamerica                    #
######################################################
######################################################
######################################################

sudamerica1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  " -15.60079,-56.06096,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de cuiaba
sudamerica2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  " -15.60079,-56.06096,2000km", time = "2005-01-01 2019-10-30")

sudamerica11<- data.frame(sudamerica1[,c(1:16,35:68,73:84)])
write.csv(sudamerica11, file = "2302tursud.csv")
sudamerica22<- data.frame(sudamerica2[,c(1:16,35:68,73:84)])
write.csv(sudamerica22, file = "2302gobsud.csv")

######################################################
######################################################
#             Tweets en America                   #
######################################################
######################################################
######################################################

norte1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "45.01627,-93.26566,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de Minneapolis, USA
norte2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "45.01627,-93.26566,2000km", time = "2005-01-01 2019-10-30")

norte11<- data.frame(norte1[,c(1:16,35:68,73:84)])
write.csv(norte11, file = "2302turnor.csv")
norte22<- data.frame(norte2[,c(1:16,35:68,73:84)])
write.csv(norte22, file = "2302gobnor.csv")

######################################################
######################################################
#             Tweets en Asia                 #
######################################################
######################################################
######################################################

asia1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "30.93019,90.32588,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de tibet, China
asia2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "30.93019,90.32588,2000km", time = "2005-01-01 2019-10-30")

asia11<- data.frame(asia1[,c(1:16,35:68,73:84)])
write.csv(asia11, file = "2302turasia.csv")
asia22<- data.frame(asia2[,c(1:16,35:68,73:84)])
write.csv(asia22, file = "2302gobasia.csv")



######################################################
######################################################
#             Tweets en Africa                   #
######################################################
######################################################
######################################################

africa1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-18.98468,32.67164,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de mutare, Zimbabwe
africa2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-18.98468,32.67164,2000km", time = "2005-01-01 2019-10-30")

africa11<- data.frame(africa1[,c(1:16,35:68,73:84)])
write.csv(africa11, file = "2302turafr.csv")
africa22<- data.frame(africa2[,c(1:16,35:68,73:84)])
write.csv(africa22, file = "2302gobafr.csv")




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
write.csv(europa11, file = "2302tureur.csv")
europa22<- data.frame(europa2[,c(1:16,35:68,73:84)])
write.csv(europa22, file = "2302gobeur.csv")

######################################################
######################################################
#             Tweets en Oceania               #
######################################################
######################################################
######################################################

oceania1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-16.26248,133.41772,2000km",time = "2005-01-01 2019-10-30")#localizacion del centro de Daly Waters, Australia
oceania2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  "-16.26248,133.41772,2000km", time = "2005-01-01 2019-10-30")

oceania11<- data.frame(oceania1[,c(1:16,35:68,73:84)])
write.csv(oceania11, file = "2302turoce.csv")
oceania22<- data.frame(oceania2[,c(1:16,35:68,73:84)])
write.csv(oceania22, file = "2302goboce.csv")


#Visualizaciones
#CORRER CODIGOS POR ORDEN



#######TENER EN CUENTA#########GRAFICA DE VIOLIN
################################COLOMBIA#####################
colombia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turcol.csv")
colombia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turcol.csv")
colombia3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turcol.csv")
colombia4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turcol.csv")
colombia5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turcol.csv")
colombia11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobcol.csv")
colombia22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobcol.csv")
colombia33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobcol.csv")
colombia44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobcol.csv")
colombia55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobcol.csv")

colGen=rbind(colombia1,colombia2,colombia3,colombia4,colombia5,colombia11,colombia22,colombia33,colombia44,colombia55) 

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
norte1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turnor.csv")
norte2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turnor.csv")
norte3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turnor.csv")
norte4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turnor.csv")
norte5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turnor.csv")
norte11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobnor.csv")
norte22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobnor.csv")
norte33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobnor.csv")
norte44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobnor.csv")
norte55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobnor.csv")

norGen=rbind(norte1,norte2,norte3,norte4,norte5,norte11,norte22,norte33,norte44,norte55) 

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
sud1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101tursud.csv")
sud2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901tursud.csv")
sud3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702tursud.csv")
sud4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502tursud.csv")
sud5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302tursud.csv")
sud11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobsud.csv")
sud22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobsud.csv")
sud33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobsud.csv")
sud44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobsud.csv")
sud55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobsud.csv")

sudGen=rbind(sud1,sud2,sud3,sud4,sud5,sud11,sud22,sud33,sud44,sud55) 

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
eur1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101tureur.csv")
eur2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901tureur.csv")
eur3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702tureur.csv")
eur4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502tureur.csv")
eur5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302tureur.csv")
eur11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobeur.csv")
eur22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobeur.csv")
eur33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobeur.csv")
eur44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobeur.csv")
eur55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobeur.csv")

eurGen=rbind(eur1,eur2,eur3,eur4,eur5,eur11,eur22,eur33,eur44,eur55) 

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
asia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turasia.csv")
asia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turasia.csv")
asia3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turasia.csv")
asia4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turasia.csv")
asia5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turasia.csv")
asia11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobasia.csv")
asia22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobasia.csv")
asia33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobasia.csv")
asia44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobasia.csv")
asia55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobasia.csv")

asiaGen=rbind(asia1,asia2,asia3,asia4,asia5,asia11,asia22,asia33,asia44,asia55) 

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
afr1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turafr.csv")
afr2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turafr.csv")
afr3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turafr.csv")
afr4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turafr.csv")
afr5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turafr.csv")
afr11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobafr.csv")
afr22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobafr.csv")
afr33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobafr.csv")
afr44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobafr.csv")
afr55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobafr.csv")

afrGen=rbind(afr1,afr2,afr3,afr4,afr5,afr11,afr22,afr33,afr44,afr55) 

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
oce1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turoce.csv")
oce2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turoce.csv")
oce3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turoce.csv")
oce4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turoce.csv")
oce5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turoce.csv")
oce11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101goboce.csv")
oce22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901goboce.csv")
oce33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702goboce.csv")
oce44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502goboce.csv")
oce55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302goboce.csv")

oceGen=rbind(oce1,oce2,oce3,oce4,oce5,oce11,oce22,oce33,oce44,oce55) 

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


png("gen2.png",width = 448,height = 289)
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
setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")

#######################################################
#######################################################
#######################################################
######################TURISMO##########################
############COLOMBIA##########
colombia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turcol.csv")
colombia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turcol.csv")
colombia3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turcol.csv")
colombia4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turcol.csv")
colombia5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turcol.csv")

colGen=rbind(colombia1,colombia2,colombia3,colombia4,colombia5)
colGen$text <- as.character(colGen$text)
sentiment_scores1 <- sentiment_by(colGen$text)

#Consruccion base de datos

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
norte1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turnor.csv")
norte2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turnor.csv")
norte3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turnor.csv")
norte4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turnor.csv")
norte5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turnor.csv")

norGen=rbind(norte1,norte2,norte3,norte4,norte5) 
norGen$text <- as.character(norGen$text)
sentiment_scores2 <- sentiment_by(norGen$text)

# Construccion base de datos
nortur<- cbind(norGen,sentiment_scores2)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "norteam.csv")


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
sud1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101tursud.csv")
sud2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901tursud.csv")
sud3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702tursud.csv")
sud4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502tursud.csv")
sud5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302tursud.csv")

sudGen=rbind(sud1,sud2,sud3,sud4,sud5) 

sudGen$text <- as.character(sudGen$text)
sentiment_scores3 <- sentiment_by(sudGen$text)
# Construccion base de datos
surtur<- cbind(sudGen,sentiment_scores3)
stur<- data.frame(surtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(stur, file = "suram.csv")

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
eur1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101tureur.csv")
eur2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901tureur.csv")
eur3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702tureur.csv")
eur4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502tureur.csv")
eur5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302tureur.csv")

eurGen=rbind(eur1,eur2,eur3,eur4,eur5) 

eurGen$text <- as.character(eurGen$text)
sentiment_scores4 <- sentiment_by(eurGen$text)
eurtur<- cbind(eurGen,sentiment_scores4)
e<- data.frame(etur[,c(2:5,7,28,47:53,57:67)])
#write.csv(e, file = "europa.csv")
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
asia1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turasia.csv")
asia2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turasia.csv")
asia3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turasia.csv")
asia4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turasia.csv")
asia5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turasia.csv")

asiaGen=rbind(asia1,asia2,asia3,asia4,asia5) 

asiaGen$text <- as.character(asiaGen$text)
sentiment_scores5 <- sentiment_by(asiaGen$text)

asitur<- cbind(asiaGen,sentiment_scores5)
a<- data.frame(asitur[,c(2:5,7,28,47:53,57:67)])
#write.csv(a, file = "asia.csv")
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
afr1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turafr.csv")
afr2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turafr.csv")
afr3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turafr.csv")
afr4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turafr.csv")
afr5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turafr.csv")

afrGen=rbind(afr1,afr2,afr3,afr4,afr5) 

afrGen$text <- as.character(afrGen$text)
sentiment_scores6 <- sentiment_by(afrGen$text)

afrtur<- cbind(afrGen,sentiment_scores6)
ntur<- data.frame(afrtur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "africa.csv")
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
oce1<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101turoce.csv")
oce2<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901turoce.csv")
oce3<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702turoce.csv")
oce4<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502turoce.csv")
oce5<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302turoce.csv")

oceGen=rbind(oce1,oce2,oce3,oce4,oce5) 

oceGen$text <- as.character(oceGen$text)
sentiment_scores7 <- sentiment_by(oceGen$text)

nortur<- cbind(oceGen,sentiment_scores7)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "oceania.csv")
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


png("tur.png",width = 448,height = 289)
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
colombia11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobcol.csv")
colombia22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobcol.csv")
colombia33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobcol.csv")
colombia44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobcol.csv")
colombia55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobcol.csv")

colGen=rbind(colombia11,colombia22,colombia33,colombia44,colombia55) 
colGen$text <- as.character(colGen$text)
sentiment_scores1 <- sentiment_by(colGen$text)

nortur<- cbind(colGen,sentiment_scores1)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "colombiagober.csv")
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
norte11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobnor.csv")
norte22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobnor.csv")
norte33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobnor.csv")
norte44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobnor.csv")
norte55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobnor.csv")

norGen=rbind(norte11,norte22,norte33,norte44,norte55) 
norGen$text <- as.character(norGen$text)
sentiment_scores2 <- sentiment_by(norGen$text)

nortur<- cbind(norGen,sentiment_scores2)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "norteamgober.csv")
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
sud11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobsud.csv")
sud22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobsud.csv")
sud33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobsud.csv")
sud44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobsud.csv")
sud55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobsud.csv")

sudGen=rbind(sud11,sud22,sud33,sud44,sud55) 

sudGen$text <- as.character(sudGen$text)
sentiment_scores3 <- sentiment_by(sudGen$text)

nortur<- cbind(sudGen,sentiment_scores3)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "suramericagober.csv")
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
eur11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobeur.csv")
eur22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobeur.csv")
eur33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobeur.csv")
eur44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobeur.csv")
eur55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobeur.csv")

eurGen=rbind(eur11,eur22,eur33,eur44,eur55) 

eurGen$text <- as.character(eurGen$text)
sentiment_scores4 <- sentiment_by(eurGen$text)

nortur<- cbind(eurGen,sentiment_scores4)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "europagober.csv")
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
asia11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobasia.csv")
asia22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobasia.csv")
asia33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobasia.csv")
asia44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobasia.csv")
asia55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobasia.csv")

asiaGen=rbind(asia11,asia22,asia33,asia44,asia55) 

asiaGen$text <- as.character(asiaGen$text)
sentiment_scores5 <- sentiment_by(asiaGen$text)

nortur<- cbind(asiaGen,sentiment_scores5)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "asiagober.csv")
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
afr11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101gobafr.csv")
afr22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901gobafr.csv")
afr33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702gobafr.csv")
afr44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502gobafr.csv")
afr55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302gobafr.csv")

afrGen=rbind(afr11,afr22,afr33,afr44,afr55) 

afrGen$text <- as.character(afrGen$text)
sentiment_scores6 <- sentiment_by(afrGen$text)

nortur<- cbind(afrGen,sentiment_scores6)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "africagober.csv")

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
oce11<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2101goboce.csv")
oce22<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2901goboce.csv")
oce33<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/0702goboce.csv")
oce44<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/1502goboce.csv")
oce55<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/2302goboce.csv")

oceGen=rbind(oce11,oce22,oce33,oce44,oce55) 

oceGen$text <- as.character(oceGen$text)
sentiment_scores7 <- sentiment_by(oceGen$text)

nortur<- cbind(oceGen,sentiment_scores7)
ntur<- data.frame(nortur[,c(2:5,7,28,47:53,57:67)])
#write.csv(ntur, file = "oceaniagober.csv")
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


png("gob.png",width = 448,height = 289)
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




###############################################################
##################Mineria de opinion############################
###############################################################
###############################################################
setwd("C:/Users/asus/Documents/Documents/Investigacion/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Serie temporal/Figuras")
#####Paso a seguir: borrar valores de 0
#https://ggplot2.tidyverse.org/reference/geom_bar.html
coltur=rbind(colombia1,colombia2,colombia3,colombia4,colombia5) 
colgob=rbind(colombia11,colombia22,colombia33,colombia44,colombia55) 
nortur=rbind(norte1,norte2,norte3,norte4,norte5)
norgob=rbind(norte11,norte22,norte33,norte44,norte55)
surtur=rbind(sud1,sud2,sud3,sud4,sud5) 
surgob=rbind(sud11,sud22,sud33,sud44,sud55) 
eurtur=rbind(eur1,eur2,eur3,eur4,eur5) 
eurgob=rbind(eur11,eur22,eur33,eur44,eur55) 
asiatur=rbind(asia1,asia2,asia3,asia4,asia5)
asiagob=rbind(asia11,asia22,asia33,asia44,asia55)
afrtur=rbind(afr1,afr2,afr3,afr4,afr5)
afrgob=rbind(afr11,afr22,afr33,afr44,afr55)
ocetur=rbind(oce1,oce2,oce3,oce4,oce5) 
ocegob=rbind(oce11,oce22,oce33,oce44,oce55) 


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
          rep("Norteam?rica-Turismo" , 16) , rep("Norteam?rica-Gobernanza" ,16),
          rep("Suram?rica-Turismo" , 16) , rep("Suram?rica-Gobernanza" ,16),
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
png("sentimiento3.png",width = 448,height = 289)
ggplot(data, aes(fill=Emociones, y=Porcentaje, x=Paises, fill=Color)) + 
  geom_bar(position="fill", stat="identity") +   scale_fill_grey() +  coord_flip()
dev.off()





#####importante-solo para el codigo rtweets- no permite de la base que se sube
split_text <- get_sentences(oceania1$text)
(emo <- emotion(oceania1$text))
emotion(split_text, drop.unused.emotions = TRUE)
plot(emo)
plot(emo, drop.unused.emotions = FALSE)
plot(emo, facet = FALSE)
plot(emo, facet = 'negated')
######################################