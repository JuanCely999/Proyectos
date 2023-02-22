##############################################################################
###Segunda parte Proyecto colciencias:Turismo y gobernanza################################
#Graficas por emociones y resumen del proyecto cuantificacion por niveles#########
######################Autor: Juan Pablo Cely#################################
###############################19-04-2020####################################

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
library(ggrepel)
cat("\f")
rm(list=ls())
################Turismo
#Col
coltur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/coltur.csv")


colneu<- filter(coltur, ave_sentiment==0 )
colneg<- filter(coltur, ave_sentiment <0 )
colpos<- filter(coltur, ave_sentiment >0 )

dim(colpos)
dim(colneg)
dim(colneu)


#sur
surtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/surtur.csv")


surneu<- filter(surtur, ave_sentiment==0 )
surneg<- filter(surtur, ave_sentiment <0 )
surpos<- filter(surtur, ave_sentiment >0 )

dim(surpos)
dim(surneg)
dim(surneu)

#nor
nortur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/nortur.csv")


norneu<- filter(nortur, ave_sentiment==0 )
norneg<- filter(nortur, ave_sentiment <0 )
norpos<- filter(nortur, ave_sentiment >0 )

dim(norpos)
dim(norneg)
dim(norneu)


#asi
asitur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/asitur.csv")


asineu<- filter(asitur, ave_sentiment==0 )
asineg<- filter(asitur, ave_sentiment <0 )
asipos<- filter(asitur, ave_sentiment >0 )

dim(asipos)
dim(asineg)
dim(asineu)

#afr
afrtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/afrtur.csv")


afrneu<- filter(afrtur, ave_sentiment==0 )
afrneg<- filter(afrtur, ave_sentiment <0 )
afrpos<- filter(afrtur, ave_sentiment >0 )

dim(afrpos)
dim(afrneg)
dim(afrneu)

#eur
eurtur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/eurtur.csv")


eurneu<- filter(eurtur, ave_sentiment==0 )
eurneg<- filter(eurtur, ave_sentiment <0 )
eurpos<- filter(eurtur, ave_sentiment >0 )

dim(eurpos)
dim(eurneg)
dim(eurneu)

#oce
ocetur<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/ocetur.csv")


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
colgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/colgober.csv")


colneu<- filter(colgober, ave_sentiment==0 )
colneg<- filter(colgober, ave_sentiment <0 )
colpos<- filter(colgober, ave_sentiment >0 )

dim(colpos)
dim(colneg)
dim(colneu)


#sur
surgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/surgober.csv")


surneu<- filter(surgober, ave_sentiment==0 )
surneg<- filter(surgober, ave_sentiment <0 )
surpos<- filter(surgober, ave_sentiment >0 )

dim(surpos)
dim(surneg)
dim(surneu)

#nor
norgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/norgober.csv")


norneu<- filter(norgober, ave_sentiment==0 )
norneg<- filter(norgober, ave_sentiment <0 )
norpos<- filter(norgober, ave_sentiment >0 )

dim(norpos)
dim(norneg)
dim(norneu)


#asi
asigober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/asigober.csv")


asineu<- filter(asigober, ave_sentiment==0 )
asineg<- filter(asigober, ave_sentiment <0 )
asipos<- filter(asigober, ave_sentiment >0 )

dim(asipos)
dim(asineg)
dim(asineu)

#afr
afrgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/afrgober.csv")


afrneu<- filter(afrgober, ave_sentiment==0 )
afrneg<- filter(afrgober, ave_sentiment <0 )
afrpos<- filter(afrgober, ave_sentiment >0 )

dim(afrpos)
dim(afrneg)
dim(afrneu)

#eur
eurgober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/eurgober.csv")


eurneu<- filter(eurgober, ave_sentiment==0 )
eurneg<- filter(eurgober, ave_sentiment <0 )
eurpos<- filter(eurgober, ave_sentiment >0 )

dim(eurpos)
dim(eurneg)
dim(eurneu)

#oce
ocegober<- read.csv("~/Documents/Investigacion/Tendencias mundiales. Gobernanza/Bases de datos final/ocegober.csv")


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





#grid.arrange(a,b,c,d,e,f)


















#africa<-cbind(afr11,ntur)

#plot(africa$retweet_count,africa$ave_sentiment)


#plot(afr211$sd,afr211$friends_count)
#plot(afr211$ave_sentiment,afr211$friends_count)

#ave_sentiment


