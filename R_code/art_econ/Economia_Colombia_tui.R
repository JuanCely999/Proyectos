##############################################################################
###Propuesta:Comentarios en Twitter de las economias latinoamericanas#####
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
library(readxl)
library(sentimentr)
#install.packages("sentiment")
#install.packages("devtools")

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
#######################FECHAS########################
#####Fecha      22-02-20 6pm     a    01-03-20 11pm 
######################################################
#Guardar Base de datos
setwd("~/Documents/Investigacion/Propuesta economia/Codigos")
#setwd("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Propuesta economia/Codigos")
##########################################################################
##########################################################################
################################COLOMBIA##################################
##########################################################################
col <- search_tweets("economia colombia", n = 1000000, include_rts = FALSE, lang="es")
col2 <- search_tweets("economy Colombia", n = 1000000, include_rts = FALSE, lang="en")

colomb1<- data.frame(col[,c(1:16,35:68,73:84)])
colomb2<- data.frame(col2[,c(1:16,35:68,73:84)])
colfinal<- rbind(colomb1,colomb2)
write.csv(colfinal, file = "1006col.csv")
colfinal2<- colfinal$text
write.csv(colfinal2, file = "1006tcol.csv")


##########################################################################
##########################################################################
################################GUATEMALA##################################
##########################################################################
gua <- search_tweets("economia guatemala", n = 1000000, include_rts = FALSE, lang="es")
gua2 <- search_tweets("economy guatemala", n = 1000000, include_rts = FALSE, lang="en")

guate1<- data.frame(gua[,c(1:16,35:68,73:84)])
guate2<- data.frame(gua2[,c(1:16,35:68,73:84)])
guafinal<- rbind(guate1,guate2)
write.csv(guafinal, file = "1006gua.csv")
guafinal2<- guafinal$text
write.csv(guafinal2, file = "1006tgua.csv")

##########################################################################
##########################################################################
################################cuba##################################
##########################################################################
cub <- search_tweets("economia cuba", n = 1000000, include_rts = FALSE, lang="es")
cub2 <- search_tweets("economy cuba", n = 1000000, include_rts = FALSE, lang="en")

cuba1<- data.frame(cub[,c(1:16,35:68,73:84)])
cuba2<- data.frame(cub2[,c(1:16,35:68,73:84)])
cubfinal<- rbind(cuba1,cuba2)
write.csv(cubfinal, file = "1006cub.csv")
cubfinal2<- cubfinal$text
write.csv(cubfinal2, file = "1006tcub.csv")

##########################################################################
##########################################################################
################################panama##################################
##########################################################################
pan <- search_tweets("economia panama", n = 1000000, include_rts = FALSE, lang="es")
pan2 <- search_tweets("economy panama", n = 1000000, include_rts = FALSE, lang="en")

pana1<- data.frame(pan[,c(1:16,35:68,73:84)])
pana2<- data.frame(pan2[,c(1:16,35:68,73:84)])
panfinal<- rbind(pana1,pana2)
write.csv(panfinal, file = "1006pan.csv")
panfinal2<- panfinal$text
write.csv(panfinal2, file = "1006tpan.csv")

##########################################################################
##########################################################################
################################ecuador##################################
##########################################################################
ecu <- search_tweets("economia ecuador", n = 1000000, include_rts = FALSE, lang="es")
ecu2 <- search_tweets("economy ecuador", n = 1000000, include_rts = FALSE, lang="en")

ecua1<- data.frame(ecu[,c(1:16,35:68,73:84)])
ecua2<- data.frame(ecu2[,c(1:16,35:68,73:84)])
ecufinal<- rbind(ecua1,ecua2)
write.csv(ecufinal, file = "1006ecu.csv")
ecufinal2<- ecufinal$text
write.csv(ecufinal2, file = "1006tecu.csv")

##########################################################################
##########################################################################
################################brasil##################################
##########################################################################
bra <- search_tweets("economia brasil", n = 1000000, include_rts = FALSE, lang="pt")#ES
bra2 <- search_tweets("economy brasil", n = 1000000, include_rts = FALSE, lang="en")

bras1<- data.frame(bra[,c(1:16,35:68,73:84)])
bras2<- data.frame(bra2[,c(1:16,35:68,73:84)])
brafinal<- rbind(bras1,bras2)
write.csv(brafinal, file = "1006bra.csv")
brafinal2<- brafinal$text
write.csv(brafinal2, file = "1006tbra.csv")

##########################################################################
##########################################################################
################################mexico##################################
##########################################################################
mex <- search_tweets("economia mexico", n = 1000000, include_rts = FALSE, lang="es")
mex2 <- search_tweets("economy mexico", n = 1000000, include_rts = FALSE, lang="en")

mexi1<- data.frame(mex[,c(1:16,35:68,73:84)])
mexi2<- data.frame(mex2[,c(1:16,35:68,73:84)])
mexfinal<- rbind(mexi1,mexi2)
write.csv(mexfinal, file = "1006mex.csv")
mexfinal2<- mexfinal$text
write.csv(mexfinal2, file = "1006tmex.csv")

##########################################################################
##########################################################################
################################bolivia##################################
##########################################################################
bol <- search_tweets("economia bolivia", n = 1000000, include_rts = FALSE, lang="es")
bol2 <- search_tweets("economy bolivia", n = 1000000, include_rts = FALSE, lang="en")

boli1<- data.frame(bol[,c(1:16,35:68,73:84)])
boli2<- data.frame(bol2[,c(1:16,35:68,73:84)])
bolfinal<- rbind(boli1,boli2)
write.csv(bolfinal, file = "1006bol.csv")
bolfinal2<- bolfinal$text
write.csv(bolfinal2, file = "1006tbol.csv")

##########################################################################
##########################################################################
################################uruguay##################################
##########################################################################
uru <- search_tweets("economia uruguay", n = 1000000, include_rts = FALSE, lang="es")
uru2 <- search_tweets("economy uruguay", n = 1000000, include_rts = FALSE, lang="en")

urug1<- data.frame(uru[,c(1:16,35:68,73:84)])
urug2<- data.frame(uru2[,c(1:16,35:68,73:84)])
urufinal<- rbind(urug1,urug2)
write.csv(urufinal, file = "1006uru.csv")
urufinal2<- urufinal$text
write.csv(urufinal2, file = "1006turu.csv")

##########################################################################
##########################################################################
################################chile##################################
##########################################################################
chi <- search_tweets("economia chile", n = 1000000, include_rts = FALSE, lang="es")
chi2 <- search_tweets("economy chile", n = 1000000, include_rts = FALSE, lang="en")

chil1<- data.frame(chi[,c(1:16,35:68,73:84)])
chil2<- data.frame(chi2[,c(1:16,35:68,73:84)])
chifinal<- rbind(chil1,chil2)
write.csv(chifinal, file = "1006chi.csv")
chifinal2<- chifinal$text
write.csv(chifinal2, file = "1006tchi.csv")



##########################################################################
##########################################################################
################################paraguay##################################
##########################################################################
par <- search_tweets("economia paraguay", n = 1000000, include_rts = FALSE, lang="es")
par2 <- search_tweets("economy paraguay", n = 1000000, include_rts = FALSE, lang="en")

para1<- data.frame(par[,c(1:16,35:68,73:84)])
para2<- data.frame(par2[,c(1:16,35:68,73:84)])
parfinal<- rbind(para1,para2)
write.csv(parfinal, file = "1006par.csv")
parfinal2<- parfinal$text
write.csv(parfinal2, file = "1006tpar.csv")

##########################################################################
##########################################################################
################################el salvador REVISAR##################################
##########################################################################
sal <- search_tweets("economia el salvador", n = 1000000, include_rts = FALSE, lang="es")
sal2 <- search_tweets("economy el salvador", n = 1000000, include_rts = FALSE, lang="en")

salv1<- data.frame(sal[,c(1:16,35:68,73:84)])
salv2<- data.frame(sal2[,c(1:16,35:68,73:84)])
salfinal<- rbind(salv1,salv2)
write.csv(salfinal, file = "1006sal.csv")
salfinal2<- salfinal$text
write.csv(salfinal2, file = "1006tsal.csv")

##########################################################################
##########################################################################
################################honduras##################################
##########################################################################
hon <- search_tweets("economia honduras", n = 1000000, include_rts = FALSE, lang="es")
hon2 <- search_tweets("economy honduras", n = 1000000, include_rts = FALSE, lang="en")

hond1<- data.frame(hon[,c(1:16,35:68,73:84)])
hond2<- data.frame(hon2[,c(1:16,35:68,73:84)])
honfinal<- rbind(hond1,hond2)
write.csv(honfinal, file = "1006hon.csv")
honfinal2<- honfinal$text
write.csv(honfinal2, file = "1006thon.csv")

##########################################################################
##########################################################################
################################peru##################################
##########################################################################
per <- search_tweets("economia peru", n = 1000000, include_rts = FALSE, lang="es")
per2 <- search_tweets("economy peru", n = 1000000, include_rts = FALSE, lang="en")

peru1<- data.frame(per[,c(1:16,35:68,73:84)])
peru2<- data.frame(per2[,c(1:16,35:68,73:84)])
perfinal<- rbind(peru1,peru2)
write.csv(perfinal, file = "1006per.csv")
perfinal2<- perfinal$text
write.csv(perfinal2, file = "1006tper.csv")

##########################################################################
##########################################################################
################################argentina##################################
##########################################################################
arg <- search_tweets("economia argentina", n = 1000000, include_rts = FALSE, lang="es")
arg2 <- search_tweets("economy argentina", n = 1000000, include_rts = FALSE, lang="en")

arge1<- data.frame(arg[,c(1:16,35:68,73:84)])
arge2<- data.frame(arg2[,c(1:16,35:68,73:84)])
argfinal<- rbind(arge1,arge2)
write.csv(argfinal, file = "1006arg.csv")
argfinal2<- argfinal$text
write.csv(argfinal2, file = "1006targ.csv")

##########################################################################
##########################################################################
################################costa rica##################################
##########################################################################
cos <- search_tweets("economia costa rica", n = 1000000, include_rts = FALSE, lang="es")
cos2 <- search_tweets("economy costa rica", n = 1000000, include_rts = FALSE, lang="en")

cost1<- data.frame(cos[,c(1:16,35:68,73:84)])
cost2<- data.frame(cos2[,c(1:16,35:68,73:84)])
cosfinal<- rbind(cost1,cost2)
write.csv(cosfinal, file = "1006cos.csv")
cosfinal2<- cosfinal$text
write.csv(cosfinal2, file = "1006tcos.csv")





##########################################################################
##########################################################################
################################venezuela##################################
##########################################################################

ven <- search_tweets("economia venezuela", n = 1000000, include_rts = FALSE, lang="es")
ven2 <- search_tweets("economy venezuela", n = 1000000, include_rts = FALSE, lang="en")

vene1<- data.frame(ven[,c(1:16,35:68,73:84)])
vene2<- data.frame(ven2[,c(1:16,35:68,73:84)])
venfinal<- rbind(vene1,vene2)
write.csv(venfinal, file = "1006ven.csv")
venfinal2<- venfinal$text
write.csv(venfinal2, file = "1006tven.csv")

##########################################################################
##########################################################################
################################dominicana##################################
##########################################################################
#Tener cuidado al correr el codigo en ingles
dom <- search_tweets("economia dominicana", n = 1000000, include_rts = FALSE, lang="es")
dom2 <- search_tweets("economy dominicana", n = 1000000, include_rts = FALSE, lang="en")

domi1<- data.frame(dom[,c(1:16,35:68,73:84)])
#domi2<- data.frame(dom2[,c(1:16,35:68,73:84)])
#domfinal<- rbind(domi1,domi2)
write.csv(domi1, file = "1006dom.csv")
domfinal1<- domi1$text
write.csv(domfinal1, file = "1006tdom.csv")

##########################################################################
##########################################################################
################################nicaragua##################################
##########################################################################

nic <- search_tweets("economia nicaragua", n = 1000000, include_rts = FALSE, lang="es")
nic2 <- search_tweets("economy nicaragua", n = 1000000, include_rts = FALSE, lang="en")

nica1<- data.frame(nic[,c(1:16,35:68,73:84)])
nica2<- data.frame(nic2[,c(1:16,35:68,73:84)])
nicfinal<- rbind(nica1,nica2)
write.csv(nicfinal, file = "1006nic.csv")
nicfinal2<- nicfinal$text
write.csv(nicfinal2, file = "1006tnic.csv")



#####Ejercicio de visualizaciones


####ejercicio de colombia#######
######HASTA QUE SE SIGA LOS SIGUIENTES PASOS
######abrir google docs excel, con solo la variable tcol
####reemplazar el texto de expa?ol a ingles
###el nombre debe coincidir con el codigo
trans<- read.csv("~/Clasificacion por tema/Trabajos/Propuesta economia/Codigos/2202tcol.csv")
traduc<- cbind(ecofinal,trans)

traduc$tra <- as.character(traduc$tra)
sentiment_scores <- sentiment_by(traduc$tra)

######################################################
a<-dim(sentiment_scores)
b<-a[1]
Lugar <- rep("Colombia" , b)
final1 <- sentiment_scores$ave_sentiment
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final1 <- data.frame(final1,Lugar)
final1 <- filter(final1, final1!=0)
names(final1)= c("Sentimiento", "Lugar")
#png("tras.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
#xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()







####ejercicio venezuela
######HASTA QUE SE SIGA LOS SIGUIENTES PASOS
######abrir google docs excel, con solo la variable tcol
####reemplazar el texto de expa?ol a ingles
###el nombre debe coincidir con el codigo
trans<- read.csv("~/Clasificacion por tema/Trabajos/Propuesta economia/Codigos/2202tven.csv")
traduc<- cbind(ecofinal,trans)

traduc$tra <- as.character(traduc$tra)
sentiment_scores <- sentiment_by(traduc$tra)

#write.csv(sentiment_scores, file = "colsent.csv")

######################################################
a<-dim(sentiment_scores)
b<-a[1]
Lugar <- rep("Venezuela" , b)
final2 <- sentiment_scores$ave_sentiment
#final1 <- trunc(final1)##Aproximar los numeros junto con floor(x)
final2 <- data.frame(final2,Lugar)
final2 <- filter(final2, final1!=0)
names(final2)= c("Sentimiento", "Lugar")
#png("tras.png",width = 448,height = 289)
#qplot(final1$Sentimiento,   geom="histogram",binwidth=0.1) + 
#xlab('Puntaje del sentimiento') + 
#ylab('Frecuencia')
#dev.off()
total=rbind(final1,final2)

ggplot(data = total, aes(x = Lugar, y = Sentimiento)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = Lugar), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Paises') + 
  ylab('Sentimiento') + guides(fill=FALSE) +
  theme_minimal()+geom_hline( yintercept=0) + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +  coord_flip()
##########################################################################
##########################################################################
##########################################################################

split_text <- get_sentences(traduc$tra)
(emo <- emotion(traduc$tra))
emotion(split_text, drop.unused.emotions = TRUE)
plot(emo)
plot(emo, drop.unused.emotions = FALSE)
plot(emo, facet = FALSE)
plot(emo, facet = 'negated')
