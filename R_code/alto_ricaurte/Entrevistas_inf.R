##############################################################################
###Entrevista articulo ######################################################
######################Autor: Juan Pablo Cely#################################
###############################09-02-2021###################################
########################################################################################################
#######                                                                                        #########
#######                     ANALISIS MUNICIPIOS ALTO RICAURTE                  #########
#######                                                                                        #########
########################################################################################################
#https://programminghistorian.org/es/lecciones/procesamiento-basico-de-textos-en-r
#http://www.aic.uva.es/AnaText/11_sentimiento.html
#https://www.tidytextmining.com/ngrams.html

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
cat("\f")
rm(list = ls())

setwd("~/Documents/Investigacion/Pedro Pablo Articulo/")



###Gachantiva

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

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
texto.gach <-Corpus(VectorSource(nov_text))
class(texto.gach)
texto.gach <- tm_map(texto.gach, tolower)
texto.gach <- tm_map(texto.gach, stripWhitespace)
texto.gach <- tm_map(texto.gach, removePunctuation)
texto.gach <- tm_map(texto.gach, removeNumbers)
texto.gach <- tm_map(texto.gach, removeWords, stopwords("spanish"))
class(texto.gach)
gach <- TermDocumentMatrix(texto.gach)
class(gach)
findFreqTerms(gach, lowfreq = 10)
findAssocs(gach, "turismo", 0.5)
findAssocs(gach, c("turismo", "gobierno"), c(0.4, 0.39))
gach.matriz <- as.matrix(gach)
data.gach <- sort(rowSums(gach.matriz), decreasing = TRUE)
head(data.gach)
data.gach <- data.frame(word = names(data.gach),freq=data.gach)
head(data.gach)










###Raquira

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(2)])

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
texto.raqu <-Corpus(VectorSource(nov_text))
class(texto.raqu)
texto.raqu <- tm_map(texto.raqu, tolower)
texto.raqu <- tm_map(texto.raqu, stripWhitespace)
texto.raqu <- tm_map(texto.raqu, removePunctuation)
texto.raqu <- tm_map(texto.raqu, removeNumbers)
texto.raqu <- tm_map(texto.raqu, removeWords, stopwords("spanish"))
class(texto.raqu)
raqu <- TermDocumentMatrix(texto.raqu)
class(raqu)
findFreqTerms(raqu, lowfreq = 10)
findAssocs(raqu, "turismo", 0.5)
findAssocs(raqu, c("turismo", "gobierno"), c(0.4, 0.39))
raqu.matriz <- as.matrix(raqu)
data.raqu <- sort(rowSums(raqu.matriz), decreasing = TRUE)
head(data.raqu)
data.raqu <- data.frame(word = names(data.raqu),freq=data.raqu)
head(data.raqu)









###sachica

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(3)])

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
texto.sach <-Corpus(VectorSource(nov_text))
class(texto.sach)
texto.sach <- tm_map(texto.sach, tolower)
texto.sach <- tm_map(texto.sach, stripWhitespace)
texto.sach <- tm_map(texto.sach, removePunctuation)
texto.sach <- tm_map(texto.sach, removeNumbers)
texto.sach <- tm_map(texto.sach, removeWords, stopwords("spanish"))
class(texto.sach)
sach <- TermDocumentMatrix(texto.sach)
class(sach)
findFreqTerms(sach, lowfreq = 10)
findAssocs(sach, "turismo", 0.5)
findAssocs(sach, c("turismo", "gobierno"), c(0.4, 0.39))
sach.matriz <- as.matrix(sach)
data.sach <- sort(rowSums(sach.matriz), decreasing = TRUE)
head(data.sach)
data.sach <- data.frame(word = names(data.sach),freq=data.sach)
head(data.sach)






###Santa Sofia

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(4)])

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
texto.sant <-Corpus(VectorSource(nov_text))
class(texto.sant)
texto.sant <- tm_map(texto.sant, tolower)
texto.sant <- tm_map(texto.sant, stripWhitespace)
texto.sant <- tm_map(texto.sant, removePunctuation)
texto.sant <- tm_map(texto.sant, removeNumbers)
texto.sant <- tm_map(texto.sant, removeWords, stopwords("spanish"))
class(texto.sant)
sant <- TermDocumentMatrix(texto.sant)
class(sant)
findFreqTerms(sant, lowfreq = 10)
findAssocs(sant, "turismo", 0.5)
findAssocs(sant, c("turismo", "gobierno"), c(0.4, 0.39))
sant.matriz <- as.matrix(sant)
data.sant <- sort(rowSums(sant.matriz), decreasing = TRUE)
head(data.sant)
data.sant <- data.frame(word = names(data.sant),freq=data.sant)
head(data.sant)



###Sutamarchan

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(5)])

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
texto.suta <-Corpus(VectorSource(nov_text))
class(texto.suta)
texto.suta <- tm_map(texto.suta, tolower)
texto.suta <- tm_map(texto.suta, stripWhitespace)
texto.suta <- tm_map(texto.suta, removePunctuation)
texto.suta <- tm_map(texto.suta, removeNumbers)
texto.suta <- tm_map(texto.suta, removeWords, stopwords("spanish"))
class(texto.suta)
suta <- TermDocumentMatrix(texto.suta)
class(suta)
findFreqTerms(suta, lowfreq = 10)
findAssocs(suta, "turismo", 0.5)
findAssocs(suta, c("turismo", "gobierno"), c(0.4, 0.39))
suta.matriz <- as.matrix(suta)
data.suta <- sort(rowSums(suta.matriz), decreasing = TRUE)
head(data.suta)
data.suta <- data.frame(word = names(data.suta),freq=data.suta)
head(data.suta)






###Tinjaca

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(6)])

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
texto.tinj <-Corpus(VectorSource(nov_text))
class(texto.tinj)
texto.tinj <- tm_map(texto.tinj, tolower)
texto.tinj <- tm_map(texto.tinj, stripWhitespace)
texto.tinj <- tm_map(texto.tinj, removePunctuation)
texto.tinj <- tm_map(texto.tinj, removeNumbers)
texto.tinj <- tm_map(texto.tinj, removeWords, stopwords("spanish"))
class(texto.tinj)
tinj <- TermDocumentMatrix(texto.tinj)
class(tinj)
findFreqTerms(tinj, lowfreq = 10)
findAssocs(tinj, "turismo", 0.5)
findAssocs(tinj, c("turismo", "gobierno"), c(0.4, 0.39))
tinj.matriz <- as.matrix(tinj)
data.tinj <- sort(rowSums(tinj.matriz), decreasing = TRUE)
head(data.tinj)
data.tinj <- data.frame(word = names(data.tinj),freq=data.tinj)
head(data.tinj)






###villa de leiva

acuerdos <- read_excel(paste("entre_alto.xlsx",sep=""),1)

acuerdos<- data.frame(acuerdos[,c(7)])

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
texto.vill <-Corpus(VectorSource(nov_text))
class(texto.vill)
texto.vill <- tm_map(texto.vill, tolower)
texto.vill <- tm_map(texto.vill, stripWhitespace)
texto.vill <- tm_map(texto.vill, removePunctuation)
texto.vill <- tm_map(texto.vill, removeNumbers)
texto.vill <- tm_map(texto.vill, removeWords, stopwords("spanish"))
class(texto.vill)
vill <- TermDocumentMatrix(texto.vill)
class(vill)
findFreqTerms(vill, lowfreq = 10)
findAssocs(vill, "turismo", 0.5)
findAssocs(vill, c("turismo", "gobierno"), c(0.4, 0.39))
vill.matriz <- as.matrix(vill)
data.vill <- sort(rowSums(vill.matriz), decreasing = TRUE)
head(data.vill)
data.vill <- data.frame(word = names(data.vill),freq=data.vill)
head(data.vill)











############################################################
############################################################
############################################################
names(data.gach)[2] <- "Gachantiva"
names(data.raqu)[2] <- "Raquira"
names(data.sach)[2] <- "Sachica"
names(data.sant)[2] <- "Santa Sofia"
names(data.suta)[2] <- "Sutamarchan"
names(data.tinj)[2] <- "Tinjaca"
names(data.vill)[2] <- "Villa de Leiva"

data.complet1<- full_join(data.gach,data.raqu)
data.complet2<- full_join(data.complet1,data.sach)
data.complet3<- full_join(data.complet2,data.sant)
data.complet4<- full_join(data.complet3,data.suta)
data.complet5<- full_join(data.complet4,data.tinj)
data.complet6<- full_join(data.complet5,data.vill)


sum(is.na(data.complet6$Gachantiva))
sum(is.na(data.complet6$Raquira))
sum(is.na(data.complet6$Sachica))
sum(is.na(data.complet6$`Santa Sofia`))
sum(is.na(data.complet6$Sutamarchan))
sum(is.na(data.complet6$Tinjaca))
sum(is.na(data.complet6$`Villa de Leiva`))


data.complet6$Gachantiva<-data.complet6$Gachantiva%>% replace_na(0)
data.complet6$Raquira<-data.complet6$Raquira%>% replace_na(0)
data.complet6$Sachica<-data.complet6$Sachica%>% replace_na(0)
data.complet6$`Santa Sofia`<-data.complet6$`Santa Sofia`%>% replace_na(0)
data.complet6$Sutamarchan<-data.complet6$Sutamarchan%>% replace_na(0)
data.complet6$Tinjaca<-data.complet6$Tinjaca%>% replace_na(0)
data.complet6$`Villa de Leiva`<-data.complet6$`Villa de Leiva`%>% replace_na(0)

head(data.complet6)
row.names(data.complet6) = data.complet6[,1]
head(data.complet6)

data.complet6 <- as.matrix(data.complet6[,-1])
head(data.complet6)

sum(is.na(data.complet6))

comparison.cloud(data.complet6)

comparison.cloud(data.complet6, max.words=500,
                 rot.per=0, scale=c(1,2.1),
                 match.colors=TRUE)




