##############################################################################
###Entrevista articulo ######################################################
######################Autor: Juan Pablo Cely#################################
###############################09-02-2021###################################
########################################################################################################
#######                                                                                        #########
#######                     ANALISIS DE REDES SOCIALES ALCALDIA DE TUNJA                   #########
#######                                                                                        #########
########################################################################################################
#https://programminghistorian.org/es/lecciones/procesamiento-basico-de-textos-en-r
#http://www.aic.uva.es/AnaText/11_sentimiento.html
#https://www.tidytextmining.com/ngrams.html
cat("\f")
rm(list = ls())
library(tidyverse)
library(tokenizers)
library(readxl)
library(tidytext)
library(dplyr)
library(janeaustenr)

library(tm) # espec?fico para miner?a de textos. 
library(SnowballC)  
library(wordcloud) #para graficar nubes de palabras  
library(ggplot2) #una gram?tica de gr?ficas que expande las funciones base de R. 
library(dplyr) # con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones m?s legibles para seres humanos.
library(readr) # facilitar? leer y escribir documentos. 
library(cluster) # con funciones para realizar an?lisis de grupos. 
library(NLP) 
library(RColorBrewer) 

library(lubridate)
library(dplyr)
library(readr)
library(rtweet)
library(wordcloud2)
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
library(gridExtra)
library(readxl)
library(oz)
library(twitteR)
library(tidyr)

setwd("~/Documents/Investigacion/Articulo Ligia /Analisis textual R/")



###Entrevista

acuerdos <- read_excel(paste("entrev.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(1)])

################################
diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)
diez <- diez[1:length(acuerdos)]

#Adem?s, convertimos a data.frame para que las columnas est?n identificadas con un nombre, lo cual ser? ?til en los siguientes pasos.
nov_text <- cbind(diez, acuerdos) %>% data.frame()

#Usamos aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras), agrupados por diez (formula = nov_raw ~ diez).
nov_text <- aggregate(formula = acuerdos ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")

#Como s?lo necesitamos la columna con los ahora p?rrafos de texto, con eso nos quedamos. Aprovechamos para transformar nov_text en una matrix, pues esto nos facilitar? los pasos siguientes.
nov_text <- nov_text %>% select(acuerdos) %>% as.matrix
dim(nov_text)

nov_text
nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
nov_text <- tolower(nov_text) #convirtiendo todo a minusculas
nov_text <- removeWords(nov_text, words = stopwords("spanish")) #eliminar palabras vacias, tales como algunas preposiciones y muletillas.
nov_text <- removePunctuation(nov_text) #se  deshace de la puntuaci?n, puesto que fin y fin. son identificadas como palabras diferentes, lo cual no deseamos.
nov_text <- removeNumbers(nov_text) #En este caso, removemos los n?meros, pues en Niebla no hay fechas y otras cantidades que deseemos conservar.
nov_text <- stripWhitespace(nov_text) #Por ?ltimo eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores.

###############          Analisis de CORPUS       ########################
#Con nuestro documento preparado, procedemos a crear nuestro Corpus, es decir, esto es nuestro acervo de documentos a analizar.
#En nuestro caso, nuestro Corpus se compone de todos los parrafos del acuerdo de paz y los asignaremos al objeto nov_corpus 
#usando las funciones VectorSource y Corpus.
texto.entrev <-Corpus(VectorSource(nov_text))
class(texto.entrev)
texto.entrev <- tm_map(texto.entrev, tolower)
texto.entrev <- tm_map(texto.entrev, stripWhitespace)
texto.entrev <- tm_map(texto.entrev, removePunctuation)
texto.entrev <- tm_map(texto.entrev, removeNumbers)
texto.entrev <- tm_map(texto.entrev, removeWords, stopwords("spanish"))
class(texto.entrev)
entrev <- TermDocumentMatrix(texto.entrev)
class(entrev)
findFreqTerms(entrev, lowfreq = 10)
findAssocs(entrev, "turismo", 0.5)
findAssocs(entrev, c("turismo", "gobierno"), c(0.4, 0.39))
entrev.matriz <- as.matrix(entrev)
data.entrev <- sort(rowSums(entrev.matriz), decreasing = TRUE)
head(data.entrev)
data.entrev <- data.frame(word = names(data.entrev),freq=data.entrev)
head(data.entrev)















###Redes sociales
red_social <- read_excel("red_social.xlsx")

acuerdos<- data.frame(red_social[,c(1)])

################################
diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)
diez <- diez[1:length(acuerdos)]

#Adem?s, convertimos a data.frame para que las columnas est?n identificadas con un nombre, lo cual ser? ?til en los siguientes pasos.
nov_text <- cbind(diez, acuerdos) %>% data.frame()

#Usamos aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras), agrupados por diez (formula = nov_raw ~ diez).
nov_text <- aggregate(formula = acuerdos ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")

#Como s?lo necesitamos la columna con los ahora p?rrafos de texto, con eso nos quedamos. Aprovechamos para transformar nov_text en una matrix, pues esto nos facilitar? los pasos siguientes.
nov_text <- nov_text %>% select(acuerdos) %>% as.matrix
dim(nov_text)

nov_text
nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
nov_text <- tolower(nov_text) #convirtiendo todo a minusculas
nov_text <- removeWords(nov_text, words = stopwords("spanish")) #eliminar palabras vacias, tales como algunas preposiciones y muletillas.
nov_text <- removePunctuation(nov_text) #se  deshace de la puntuaci?n, puesto que fin y fin. son identificadas como palabras diferentes, lo cual no deseamos.
nov_text <- removeNumbers(nov_text) #En este caso, removemos los n?meros, pues en Niebla no hay fechas y otras cantidades que deseemos conservar.
nov_text <- stripWhitespace(nov_text) #Por ?ltimo eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores.

###############          Analisis de CORPUS       ########################
#Con nuestro documento preparado, procedemos a crear nuestro Corpus, es decir, esto es nuestro acervo de documentos a analizar.
#En nuestro caso, nuestro Corpus se compone de todos los parrafos del acuerdo de paz y los asignaremos al objeto nov_corpus 
#usando las funciones VectorSource y Corpus.
texto.social <-Corpus(VectorSource(nov_text))
class(texto.social)
texto.social <- tm_map(texto.social, tolower)
texto.social <- tm_map(texto.social, stripWhitespace)
texto.social <- tm_map(texto.social, removePunctuation)
texto.social <- tm_map(texto.social, removeNumbers)
texto.social <- tm_map(texto.social, removeWords, stopwords("spanish"))
class(texto.social)
social <- TermDocumentMatrix(texto.social)
class(social)
findFreqTerms(social, lowfreq = 10)
findAssocs(social, "turismo", 0.5)
findAssocs(social, c("turismo", "gobierno"), c(0.4, 0.39))
social.matriz <- as.matrix(social)
data.social <- sort(rowSums(social.matriz), decreasing = TRUE)
head(data.social)
data.social <- data.frame(word = names(data.social),freq=data.social)
head(data.social)






















###Plan de desarrollo
total <- read_excel(paste("entrev.xlsx",sep=""),4)

acuerdos<- data.frame(total[,c(1)])

################################
diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)
diez <- diez[1:length(acuerdos)]

#Adem?s, convertimos a data.frame para que las columnas est?n identificadas con un nombre, lo cual ser? ?til en los siguientes pasos.
nov_text <- cbind(diez, acuerdos) %>% data.frame()

#Usamos aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras), agrupados por diez (formula = nov_raw ~ diez).
nov_text <- aggregate(formula = acuerdos ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")

#Como s?lo necesitamos la columna con los ahora p?rrafos de texto, con eso nos quedamos. Aprovechamos para transformar nov_text en una matrix, pues esto nos facilitar? los pasos siguientes.
nov_text <- nov_text %>% select(acuerdos) %>% as.matrix
dim(nov_text)

nov_text
nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
nov_text <- tolower(nov_text) #convirtiendo todo a minusculas
nov_text <- removeWords(nov_text, words = stopwords("spanish")) #eliminar palabras vacias, tales como algunas preposiciones y muletillas.
nov_text <- removePunctuation(nov_text) #se  deshace de la puntuaci?n, puesto que fin y fin. son identificadas como palabras diferentes, lo cual no deseamos.
nov_text <- removeNumbers(nov_text) #En este caso, removemos los n?meros, pues en Niebla no hay fechas y otras cantidades que deseemos conservar.
nov_text <- stripWhitespace(nov_text) #Por ?ltimo eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores.

###############          Analisis de CORPUS       ########################
#Con nuestro documento preparado, procedemos a crear nuestro Corpus, es decir, esto es nuestro acervo de documentos a analizar.
#En nuestro caso, nuestro Corpus se compone de todos los parrafos del acuerdo de paz y los asignaremos al objeto nov_corpus 
#usando las funciones VectorSource y Corpus.
texto.plan <-Corpus(VectorSource(nov_text))
class(texto.plan)
texto.plan <- tm_map(texto.plan, tolower)
texto.plan <- tm_map(texto.plan, stripWhitespace)
texto.plan <- tm_map(texto.plan, removePunctuation)
texto.plan <- tm_map(texto.plan, removeNumbers)
texto.plan <- tm_map(texto.plan, removeWords, stopwords("spanish"))
class(texto.plan)
plan <- TermDocumentMatrix(texto.plan)
class(plan)
findFreqTerms(plan, lowfreq = 10)
findAssocs(plan, "turismo", 0.5)
findAssocs(plan, c("turismo", "gobierno"), c(0.4, 0.39))
plan.matriz <- as.matrix(plan)
data.plan <- sort(rowSums(plan.matriz), decreasing = TRUE)
head(data.plan)
data.plan <- data.frame(word = names(data.plan),freq=data.plan)
head(data.plan)


############################################################
############################################################
############################################################
names(data.entrev)[2] <- "Entrevista"
names(data.social)[2] <- "Red_social"
names(data.plan)[2] <- "Plan_desarrollo"

data.complet <- full_join(data.entrev, data.social)
data.completa <- full_join(data.complet, data.plan)

sum(is.na(data.completa$Entrevista))
sum(is.na(data.completa$Red_social))
sum(is.na(data.completa$Plan_desarrollo))

data.completa$Entrevista <- data.completa$Entrevista %>% replace_na(0)
data.completa$Red_social <- data.completa$Red_social %>% replace_na(0)
data.completa$Plan_desarrollo <- data.completa$Plan_desarrollo %>% replace_na(0)

sum(is.na(data.completa$Entrevista))

head(data.completa)
row.names(data.completa) = data.completa[,1]
head(data.completa)

data.completa <- as.matrix(data.completa[,-1])
head(data.completa)

sum(is.na(data.completa))

comparison.cloud(data.completa)

comparison.cloud(data.completa, max.words=300,
            rot.per=0, scale=c(5,0.1),
                 match.colors=TRUE)




#####################################333
#OTROS DATOS
head(data.entrev,20)
head(data.social,20)
head(data.plan,20)


