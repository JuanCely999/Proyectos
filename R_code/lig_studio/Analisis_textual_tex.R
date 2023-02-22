##############################################################################
###Entrevista articulo ######################################################
######################Autor: Juan Pablo Cely#################################
###############################25-07-2020###################################
########################################################################################################
#######                                                                                        #########
#######                     ANALISIS DE TEXTO ENTREVISTAS                    #########
#######                                                                                        #########
########################################################################################################

# mas info en "http://www.gutenberg.org/ebooks/49836?msg=welcome_stranger"

#################################################
#setwd("C:/Users/emartigo/Desktop/Eduard Fernando Martinez Gonzalez/R")
#uber_txt <- read_lines("~/Documents/Investigacion/Articulo Ligia /Analisis textual R/DII.txt", skip = 1, n_max = 218-1 ) #135-1 entrevistas CD,218-1 entrevistas escritas leyendo el documento 135-0.txt desde la linea 1 hasta la linea 135
cat("\f")
rm(list = ls())

#SE NECESITAN LOS PAQUETES 


###########AJUSTAR 127###


#**************************
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

setwd("~/Documents/Investigacion/Articulo Ligia /Analisis textual R/")
#####Entrevista
#acuerdos <- read_excel("entrev.xlsx")
#####Plan de desarrollo de Tunja
acuerdos <- read_excel(paste("entrev.xlsx",sep=""),4)

acuerdos<- data.frame(acuerdos[,c(1)])



#l parte

################################
#acuerdos <- read_lines("~/Documents/Investigacion/Articulo Ligia /Analisis textual R/DII.txt", skip = 1, n_max = 218-1 ) #135-1 entrevistas CD,218-1 entrevistas escritas leyendo el documento 135-0.txt desde la linea 1 hasta la linea 135
str(acuerdos)  #El objeto acuerdo que obtuvimos es uno de tipo character, con 32937 elementos.


diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)

# sep='\t'  esto quiere decir que esta separado por tabulaciones

#De este vector, nos quedamos con un n?mero de elementos igual al n?mero de renglones del objeto nov_raw (length(nov_raw)), para facilitar combinarlos.

diez <- diez[1:length(acuerdos)]


#Combinamos diez con now_raw y los asignamos al objeto nov_text. As? tenemos una columna con los renglones de texto y otra con un n?mero que identifica a qu? grupo de diez renglones pertenece.
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
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus


########################        Nube de palabras      ########################
#Mapearemos nuestro Corpus como un documento de texto plano usando las funciones tm_map y PlainTextDocument).
nov_ptd <- tm_map(nov_corpus, PlainTextDocument)

nov_tdm <- TermDocumentMatrix(nov_ptd)
nov_mat <- as.matrix(nov_tdm)
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)





# Con nuestro Corpus mapeado de esta manera, podemos crear f?cilmente una nube de palabras (wordcloud de la librer?a del mismo nombre) que nos muestro los t?rminos m?s frecuentes en Niebla.
#wordcloud(nov_ptd, min.freq = 1, max.words = 250, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))
wordcloud2(nov_mat, size=3, color='random-dark', shape = 'circle')




########################        Mas depuraci?n      ########################

nov_text <- removeWords(nov_text, words = c("creo","cada","leo","lee","pero","nadie","que", "una" , "los" , "por" , "del" , "muchos" , "tengo" , "cosas" , "mas" ,"muy" , "buena" , "las" , "los" , "sobre", "tener" , "pues", "todo", "sus","con","tambien","fue","mis","forma","una","porque","cuenta","gusta","por","como","para","mucho","acerca","son","nos","uno","como","que","asi","hacia","osea","hay","bueno","uno","como","que"))
nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)




########################          Term Document Matrix        ######################## 
#Mapearemos nuestro Corpus indicando que es una matriz de t?rminos, de esta manera podremos hacer realizar operaciones como identificar asociaciones entre palabras.
# Usaremos la funci?n TermDocumentMatrix en nuestro Corpus y asignaremos el resultado al objeto nov_tdm.
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm




########################         Frecuencia de palabras              ##########################
#Aunque una nube de palabras nos muestra de manera visual la frecuencia de las palabras en nuestro Corpus, no nos devuelve cantidades.
#Para obtenerlas, primero transformaremos nuestro objeto nov_tdm en un objeto de clase matrix, que de nuevo tendr? un n?mero de renglones igual al n?mero de palabras distintas de nuestro Corpus y n?mero de columnas igual a su n?mero de documentos.
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

# Obtenemos las sumas de renglones (rowSums) odenadas de mayor a menor (sort con decreasing = TRUE)para conocer la frecuencia de cada palabra y despu?s transformamos 
#los resultados a objeto de clase data.frame de dos columnas, palabra y frec, que nos permitir? graficar f?cilmente su contenido.
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

# Graficando este nuevo objeto
wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, min.freq = 1,
  max.words = 100, 
  random.order = F, scale = c(2,0.5), rot.per = 0,
  colors=brewer.pal(name = "Dark2", n = 8))


nov_mat[1:100,]
repetidas2 <- data.frame(nov_mat[1:100,])
#install.packages("xlsx")
#library(xlsx)
write_excel_csv(repetidas2, "repetidas2.csv")
# https://www.dropbox.com/s/16jom42nihraawf/Analisis_de_los_Nuevos_Acuerdos_de_Paz.R?dl=0

 



##Graficas en el tiempo de la oració

cat("\f")
rm(list = ls())


total <- read_excel(paste("entrev.xlsx",sep=""),2)

#Plot lineas
#Pregunta1
total$text1 <- as.character(total$text1)
a1<-sentiment(total$text1)
b1<-sentiment_by(total$text1)
p1<-plot(a1)+ ggtitle("1")
a1
b1
#Pregunta2
total$text2 <- as.character(total$text2)
a2<-sentiment(total$text2)
b2<-sentiment_by(total$text2)
p2<-plot(a2)+ ggtitle("2")
a2
b2
#Pregunta3
total$text3 <- as.character(total$text3)
a3<-sentiment(total$text3)
b3<-sentiment_by(total$text3)
p3<-plot(a3)+ ggtitle("3")
a3
b3
#Pregunta4
total$text4 <- as.character(total$text4)
a4<-sentiment(total$text4)
b4<-sentiment_by(total$text4)
p4<-plot(a4)+ ggtitle("4")
a4
b4
#Pregunta5
total$text5 <- as.character(total$text5)
a5<-sentiment(total$text5)
b5<-sentiment_by(total$text5)
p5<-plot(a5)+ ggtitle("5")
a5
b5
#Pregunta6
total$text6 <- as.character(total$text6)
a6<-sentiment(total$text6)
b6<-sentiment_by(total$text6)
p6<-plot(a6)+ ggtitle("6")
a6
b6
#Pregunta7
total$text7 <- as.character(total$text7)
a7<-sentiment(total$text7)
b7<-sentiment_by(total$text7)
p7<-plot(a7)+ ggtitle("7")
a7
b7
#Pregunta8
total$text8 <- as.character(total$text8)
a8<-sentiment(total$text8)
b8<-sentiment_by(total$text8)
p8<-plot(a8)+ ggtitle("8")
a8
b8
#Pregunta9
total$text9 <- as.character(total$text9)
a9<-sentiment(total$text9)
b9<-sentiment_by(total$text9)
p9<-plot(a9)+ ggtitle("9")
a9
b9

#General
gen <- read_excel(paste("entrev.xlsx",sep=""),3)
a10<-sentiment(gen$general)
b10<-sentiment_by(total$text9)
p10<-plot(a10)+ ggtitle("General")
a10
b10

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)


split_text <- get_sentences(gen$general)
(emo <- emotion(gen$general))
emotion(split_text, drop.unused.emotions = TRUE)
plot(emo)
plot(emo, drop.unused.emotions = FALSE)
plot(emo, facet = FALSE)
plot(emo, facet = 'negated')


 


####################################################
##############Barras de sentimientos################
####################################################
cat("\f")
rm(list = ls())

total <- read_excel(paste("entrev.xlsx",sep=""),2)
#Pregunta 1
total$text1 <- as.character(total$text1)
sentiment_scores1 <- emotion(total$text1)
sentiment_scores1 <- filter(sentiment_scores1, emotion_count!=0)
a<-dim(sentiment_scores1)
b<-a[1]
Lugar <- rep("Pregunta1" , b)
final1 <- sentiment_scores1$emotion_type
final99 <- data.frame(final1,Lugar)
#Pregunta 2
total$text2 <- as.character(total$text2)
sentiment_scores2 <- emotion(total$text2)
sentiment_scores2 <- filter(sentiment_scores2, emotion_count!=0)
a<-dim(sentiment_scores2)
b<-a[1]
Lugar <- rep("nor" , b)
final1 <- sentiment_scores2$emotion_type
final2 <- data.frame(final1,Lugar)
#Pregunta3
total$text3 <- as.character(total$text3)
sentiment_scores3 <- emotion(total$text3)
sentiment_scores3 <- filter(sentiment_scores3, emotion_count!=0)
a<-dim(sentiment_scores3)
b<-a[1]
Lugar <- rep("sur1" , b)
final1 <- sentiment_scores3$emotion_type
final3 <- data.frame(final1,Lugar)
#Pregunta4
total$text4 <- as.character(total$text4)
sentiment_scores4 <- emotion(total$text4)
sentiment_scores4 <- filter(sentiment_scores4, emotion_count!=0)
a<-dim(sentiment_scores4)
b<-a[1]
Lugar <- rep("eur1" , b)
final1 <- sentiment_scores4$emotion_type
final4 <- data.frame(final1,Lugar)
#Pregunta5
total$text5 <- as.character(total$text5)
sentiment_scores5 <- emotion(total$text5)
sentiment_scores5 <- filter(sentiment_scores5, emotion_count!=0)
a<-dim(sentiment_scores5)
b<-a[1]
Lugar <- rep("asia1" , b)
final1 <- sentiment_scores5$emotion_type
final5 <- data.frame(final1,Lugar)
#Pregunta6 
total$text6 <- as.character(total$text6)
sentiment_scores6 <- emotion(total$text6)
sentiment_scores6 <- filter(sentiment_scores6, emotion_count!=0)
a<-dim(sentiment_scores6)
b<-a[1]
Lugar <- rep("afr1" , b)
final1 <- sentiment_scores6$emotion_type
final6 <- data.frame(final1,Lugar)
#Pregunta7
total$text7 <- as.character(total$text7)
sentiment_scores7 <- emotion(total$text7)
sentiment_scores7 <- filter(sentiment_scores7, emotion_count!=0)
a<-dim(sentiment_scores7)
b<-a[1]
Lugar <- rep("oce1" , b)
final1 <- sentiment_scores7$emotion_type
final7 <- data.frame(final1,Lugar)
#Pregunta8
total$text8 <- as.character(total$text8)
sentiment_scores8 <- emotion(total$text8)
sentiment_scores8 <- filter(sentiment_scores8, emotion_count!=0)
a<-dim(sentiment_scores8)
b<-a[1]
Lugar <- rep("oce1" , b)
final1 <- sentiment_scores8$emotion_type
final8 <- data.frame(final1,Lugar)
#Pregunta9
total$text9 <- as.character(total$text9)
sentiment_scores9 <- emotion(total$text9)
sentiment_scores9 <- filter(sentiment_scores9, emotion_count!=0)
a<-dim(sentiment_scores9)
b<-a[1]
Lugar <- rep("oce1" , b)
final1 <- sentiment_scores9$emotion_type
final9 <- data.frame(final1,Lugar)

com1<-data.frame(table(final99$final1))#relevante para tesis
com2<-data.frame(table(final2$final1))
com3<-data.frame(table(final3$final1))
com4<-data.frame(table(final4$final1))
com5<-data.frame(table(final5$final1))
com6<-data.frame(table(final6$final1))
com7<-data.frame(table(final7$final1))
com8<-data.frame(table(final8$final1))
com9<-data.frame(table(final9$final1))

union <- rbind(com1,com2,com3,com4,com5,com6,com7,com8,com9)

preguntas <- c(rep("1" , 16) , rep("2" ,16),
          rep("3" , 16) , rep("4" ,16),
          rep("5" , 16) , rep("6" ,16),
          rep("7" , 16) , rep("8" ,16),
          rep("9" , 16))

data <- data.frame(preguntas,union)
data <- filter(data, Freq!=0)
data <- filter(data, Var1!="anger_negated")
data <- filter(data, Var1!="anticipation_negated")
data <- filter(data, Var1!="disgust_negated")
data <- filter(data, Var1!="fear_negated")
data <- filter(data, Var1!="joy_negated")
data <- filter(data, Var1!="surprise_negated")
data <- filter(data, Var1!="trust_negated")
data <- filter(data, Var1!="sadness_negated")

data = rename(data, c(Var1="Emociones", Freq="Porcentaje"))
names (data)
png("preguntas.png",width = 600,height = 400)
ggplot(data, aes(fill=Emociones, y=Porcentaje, x=preguntas, fill=Color)) + 
  geom_bar(position="fill", stat="identity")+   scale_fill_grey()  +  coord_flip()
dev.off()







cat("\f")
rm(list = ls())

#Analisis de Twitter de la Alcaldia de Tunja

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
###############################################



colombia1 <- search_tweets("@AlcaldiaTunja", n = 1000000,include_rts = FALSE )
#Nubes de Palabras de Tweets de la alcaldia de Tunja

colombia11<- data.frame(colombia1[,c(5)])
write.csv(colombia11, file = "alcaltunja.csv")
acuerdos<-colombia11$text


dim(colombia11)  #El objeto acuerdo que obtuvimos es uno de tipo character, con 32937 elementos.


diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)

# sep='\t'  esto quiere decir que esta separado por tabulaciones

#De este vector, nos quedamos con un n?mero de elementos igual al n?mero de renglones del objeto nov_raw (length(nov_raw)), para facilitar combinarlos.

diez <- diez[1:length(acuerdos)]


#Combinamos diez con now_raw y los asignamos al objeto nov_text. As? tenemos una columna con los renglones de texto y otra con un n?mero que identifica a qu? grupo de diez renglones pertenece.
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
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus


########################        Nube de palabras      ########################
#Mapearemos nuestro Corpus como un documento de texto plano usando las funciones tm_map y PlainTextDocument).
nov_ptd <- tm_map(nov_corpus, PlainTextDocument)
# Con nuestro Corpus mapeado de esta manera, podemos crear f?cilmente una nube de palabras (wordcloud de la librer?a del mismo nombre) que nos muestro los t?rminos m?s frecuentes en Niebla.
wordcloud(nov_ptd, min.freq = 1, max.words = 250, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))





########################        Mas depuraci?n      ########################

nov_text <- removeWords(nov_text, words = c("creo","cada","leo","lee","pero","nadie","que", "una" , "los" , "por" , "del" , "muchos" , "tengo" , "cosas" , "mas" ,"muy" , "buena" , "las" , "los" , "sobre", "tener" , "pues", "todo", "sus","con","tambien","fue","mis","forma","una","porque","cuenta","gusta","por","como","para","mucho","acerca","son","nos","uno","como","que","asi","hacia","osea","hay","bueno","uno","como","que"))
nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)




########################          Term Document Matrix        ######################## 
#Mapearemos nuestro Corpus indicando que es una matriz de t?rminos, de esta manera podremos hacer realizar operaciones como identificar asociaciones entre palabras.
# Usaremos la funci?n TermDocumentMatrix en nuestro Corpus y asignaremos el resultado al objeto nov_tdm.
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm




########################         Frecuencia de palabras              ##########################
#Aunque una nube de palabras nos muestra de manera visual la frecuencia de las palabras en nuestro Corpus, no nos devuelve cantidades.
#Para obtenerlas, primero transformaremos nuestro objeto nov_tdm en un objeto de clase matrix, que de nuevo tendr? un n?mero de renglones igual al n?mero de palabras distintas de nuestro Corpus y n?mero de columnas igual a su n?mero de documentos.
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

# Obtenemos las sumas de renglones (rowSums) odenadas de mayor a menor (sort con decreasing = TRUE)para conocer la frecuencia de cada palabra y despu?s transformamos 
#los resultados a objeto de clase data.frame de dos columnas, palabra y frec, que nos permitir? graficar f?cilmente su contenido.
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

# Graficando este nuevo objeto
wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, min.freq = 1,
  max.words = 100, 
  random.order = F, scale = c(3,0.5), rot.per = 0,
  colors=brewer.pal(name = "Dark2", n = 8))




######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
cat("\f")
rm(list = ls())
##############Entrevista
#traduc <- read_excel("Traduc.xlsx")
#Plan de desarrollo de Tunja
traduc <- read_excel(paste("entrev.xlsx",sep=""),4)




split_text <- get_sentences(traduc$text2)
(emo <- emotion(traduc$text2))
emotion(split_text, drop.unused.emotions = TRUE)
plot(emo)
plot(emo, drop.unused.emotions = FALSE)
plot(emo, facet = FALSE)
plot(emo, facet = 'negated')

traduc$text2 <- as.character(traduc$text2)
sentiment_scores <- sentiment(traduc$text2)
a<-sentiment_by(traduc$text2)
plot(sentiment_scores)

sd(a$ave_sentiment)
mean(a$ave_sentiment)

