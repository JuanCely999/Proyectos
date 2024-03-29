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


# Accedemos a Twitter a trav�s de los datos del token
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

######################################################
######################################################
######################################################
#             Tweets en Colombia                    #
######################################################
######################################################
######################################################

uber_tweets1 <- search_tweets("tourism", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja
uber_tweets2 <- search_tweets("governance", n = 1000000, include_rts = FALSE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja
#uber_tweets <- search_tweets("turismo", n = 10000, include_rts = FALSE, lang="es", geocode =  " 5.55148,-73.35688,600km")#localizacion del centro de Tunja
#table(uber_tweets1$screen_name)

Datosss = data.frame(uber_tweets1[,c(1:16,35:68,73:84)])
Dato = data.frame(Datoss[,-c(17)])
setwd("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Princ Tweet")
write.csv(Datosss, file = "dosresultados2.csv")





##################################################
##################################################
##################################################
#  top 5 de los usuarios m�s populares (con m�s seguidores), su procedencia, y el contenido del tweet
#######turismo######
view( uber_tweets1 %>% 
        top_n(5, followers_count) %>% 
        arrange(desc(followers_count)) %>% 
        select(screen_name, followers_count, location, text))
tw1 <- search_tweets("tourism", n = 1000000, include_rts = TRUE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja

a<- tw1 %>% 
        filter(!is_retweet) %>% 
        filter(retweet_count == max(retweet_count)) %>% 
        select(screen_name, retweet_count, followers_count, location, text)
setwd("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Princ Tweet")
write.csv(a, file="turcol.csv")

#############################
############Gobernanza
a<-uber_tweets2 %>% 
        top_n(5, followers_count) %>% 
        arrange(desc(followers_count)) %>% 
        select(screen_name, followers_count, location, text)
write.csv(a, file="top5gov.csv")
tw2 <- search_tweets("governance", n = 1000000, include_rts = TRUE, lang="en", geocode =  " 5.55148,-73.35688,800km")#localizacion del centro de Tunja



a<- tw2 %>% 
  filter(!is_retweet) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  select(screen_name, retweet_count, followers_count, location, text)
setwd("C:/Users/asus/Documents/Clasificacion por tema/Trabajos/Tendencias mundiales. Gobernanza/Simulacros en Web Scrapping/Princ Tweet")
write.csv(a, file="govcol.csv")



##########################################################################
##########################Parar##########################################

#sujeto a editarlo ya que puede cambiar las observaciones
Categoria <- rep("Tourism" , 139)
uber_tweets11 <- data.frame(uber_tweets1,Categoria)
Categoria <- rep("Governance" , 73)
uber_tweets22 <- data.frame(uber_tweets2,Categoria)
total=rbind(uber_tweets11,uber_tweets22)

table(uber_tweets1$source)
#table(uber_tweets2$screen_name)
table(uber_tweets2$source)

dim(uber_tweets1)
dim(uber_tweets2)
####################TURISMO Y GOBERNANZA#####################
dim(total)
head(total$text)
library(sentimentr)

total$text <- as.character(total$text)

sentiment_scores <- sentiment(total$text)
sentiment_by(total$text)
plot(sentiment_scores)
head(sentiment_scores)
#View(sentiment(total$text))

library(ggrepel)
library(tidyverse)
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}
plot(sentiment_scores)+geom_line(size=0.1)#Aumentar tama�o de la letra


# Average the scores across all comments
average_sentiment_score <- sum(sentiment_scores$sentiment)/length(sentiment_scores$sentiment)
average_sentiment_score
#0.07257931


##################TURISMO################
head(uber_tweets11$text)
uber_tweets11$text <- as.character(uber_tweets11$text)

#View(sentiment(uber_tweets11$text))
sentiment_scores1 <- sentiment(uber_tweets11$text)
plot(sentiment_scores1) +geom_line(size=0.1)#Aumentar tama�o de la letra
average_sentiment_score <- sum(sentiment_scores1$sentiment)/length(sentiment_scores$sentiment)
average_sentiment_score
#0.05967945

##################Gobernanza################
head(uber_tweets22$text)

uber_tweets22$text <- as.character(uber_tweets22$text)

sentiment_scores2 <- sentiment(uber_tweets22$text)
head(sentiment_scores2)
#View(sentiment(uber_tweets22$text))
plot(sentiment_scores2)+geom_line(size=0.1)#Aumentar tama�o de la letra

average_sentiment_score <- sum(sentiment_scores2$sentiment)/length(sentiment_scores$sentiment)
average_sentiment_score
#0.01289985

sentimiento1<- sentiment_scores1$sentiment
sentimiento2<- sentiment_scores2$sentiment

###############Sujeto a cambios##############
#total
dim(sentiment_scores1)
dim(sentiment_scores2)
Categoria <- rep("Turismo" , 274)
x<- 1:274
final1 <- data.frame(Categoria,x,sentimiento1)
names(final1)= c("Categorias", "x", "Sentimiento")
#names(final2)= c("Categorias", "Costos")
Categoria <- rep("Gobernanza" , 152)
x<- 1:152
final2 <- data.frame(Categoria,x,sentimiento2)
names(final2)= c("Categorias","x", "Sentimiento")
total=rbind(final1,final2)

ggplot(total, aes(x=x, y=Sentimiento, color=Categorias)) + 
  geom_point(size=2) +
  theme_ipsum()

















#https://rpubs.com/Jo_/GraficoBase
#####################Word cloud##################
#_________________________________________________________________#
#_________________________________________________________________#
###############turismo###########################


uber_txt <- uber_tweets1$text
# Otra forma de coger el texto
# some_txt = sapply(some_tweets, function(x) x$getText())
# Mostramos los 10 primeros
head(uber_txt, 10)
uber_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", uber_txt)

# Veamos los 10 primeros tweets
head(uber_txt, 10)

uber_txt <- gsub("@\\w+", "", uber_txt)

# Ahora veremos 5 tweets
head(uber_txt, 5)

uber_txt <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", uber_txt)
head(uber_txt, 5)


uber_txt <- gsub("[^a-zA-Z0-9 ]", "", uber_txt)
head(uber_txt, 5)

uber_txt <- gsub("[[:punct:]]", "", uber_txt)
head(uber_txt, 5)

uber_txt <- gsub("amp ", "", uber_txt)
head(uber_txt, 5)



uber_txt <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", uber_txt)
head(uber_txt, 5)
uber_txt <- uber_txt[!is.na(uber_txt)]
head(uber_txt, 5)
uber_txt <- iconv(uber_txt, 'UTF-8', 'ASCII')
head(uber_txt, 5)
uber_txt <- gsub("[ \t]{2,}", "", uber_txt)
uber_txt <- gsub("^\\s+|\\s+$", "", uber_txt)
head(uber_txt, 5)
uber_txt <- tolower(uber_txt)
head(uber_txt, 5)
head(uber_tweets1$text, 5)
#View(uber_tweets)
head(uber_txt, 5)
#library(tm)
uber_corpus <- Corpus(VectorSource(uber_txt))
inspect(uber_corpus[1:10])
uber_corpus_clean <- tm_map(uber_corpus, tolower)
uber_corpus_clean <- tm_map(uber_corpus_clean, removeNumbers)
uber_corpus_clean <- tm_map(uber_corpus_clean, removePunctuation)
uber_corpus_clean <- tm_map(uber_corpus_clean, stripWhitespace)
stopwords(kind='en')#en ingles, es espa�ol
uber_corpus_clean <- tm_map(uber_corpus_clean, removeWords, stopwords(kind = "en"))
uber_tdm           <- TermDocumentMatrix(uber_corpus_clean, control = list(stopwords = TRUE))
uber_tdm           <- as.matrix(uber_tdm)

uber_dtm <- DocumentTermMatrix(uber_corpus_clean, control = list(minWordLength = 1, stopwords = TRUE))
inspect(uber_dtm)
uber_corpus_stem <- tm_map(uber_corpus_clean, stemDocument)
uber_corpus_stem <- tm_map(uber_corpus_stem, stemCompletion, dictionary = uber_corpus_clean)
inspect(uber_corpus_stem[1:5])
head(findFreqTerms(uber_dtm, lowfreq=10), 40)
findAssocs(uber_dtm, 'cabify', 0.40)
Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
uber_dict  <- Dictionary(findFreqTerms(uber_dtm, 5))
head(uber_dict)
library(wordcloud)
#colores=brewer.pal(8,"Dark2")
wordcloud(uber_corpus_clean, random.order = FALSE, scale = c(3,0.5),
          max.words = 75,rot.per = 0.25, colors=brewer.pal(name = "Dark2", n = 8))
#










#################################################
##################Gobernanza#####################


uber_txt <- uber_tweets2$text
# Otra forma de coger el texto
# some_txt = sapply(some_tweets, function(x) x$getText())
# Mostramos los 10 primeros
head(uber_txt, 10)
uber_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", uber_txt)

# Veamos los 10 primeros tweets
head(uber_txt, 10)

uber_txt <- gsub("@\\w+", "", uber_txt)

# Ahora veremos 5 tweets
head(uber_txt, 5)

uber_txt <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", uber_txt)
head(uber_txt, 5)


uber_txt <- gsub("[^a-zA-Z0-9 ]", "", uber_txt)
head(uber_txt, 5)

uber_txt <- gsub("[[:punct:]]", "", uber_txt)
head(uber_txt, 5)

uber_txt <- gsub("amp ", "", uber_txt)
head(uber_txt, 5)



uber_txt <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", uber_txt)
head(uber_txt, 5)
uber_txt <- uber_txt[!is.na(uber_txt)]
head(uber_txt, 5)
uber_txt <- iconv(uber_txt, 'UTF-8', 'ASCII')
head(uber_txt, 5)
uber_txt <- gsub("[ \t]{2,}", "", uber_txt)
uber_txt <- gsub("^\\s+|\\s+$", "", uber_txt)
head(uber_txt, 5)
uber_txt <- tolower(uber_txt)
head(uber_txt, 5)
head(uber_tweets2$text, 5)
#View(uber_tweets)
head(uber_txt, 5)
#library(tm)
uber_corpus <- Corpus(VectorSource(uber_txt))
inspect(uber_corpus[1:10])
uber_corpus_clean <- tm_map(uber_corpus, tolower)
uber_corpus_clean <- tm_map(uber_corpus_clean, removeNumbers)
uber_corpus_clean <- tm_map(uber_corpus_clean, removePunctuation)
uber_corpus_clean <- tm_map(uber_corpus_clean, stripWhitespace)
stopwords(kind='en')#en ingles, es espa�ol
uber_corpus_clean <- tm_map(uber_corpus_clean, removeWords, stopwords(kind = "en"))
uber_tdm           <- TermDocumentMatrix(uber_corpus_clean, control = list(stopwords = TRUE))
uber_tdm           <- as.matrix(uber_tdm)

uber_dtm <- DocumentTermMatrix(uber_corpus_clean, control = list(minWordLength = 1, stopwords = TRUE))
inspect(uber_dtm)
uber_corpus_stem <- tm_map(uber_corpus_clean, stemDocument)
uber_corpus_stem <- tm_map(uber_corpus_stem, stemCompletion, dictionary = uber_corpus_clean)
inspect(uber_corpus_stem[1:5])
head(findFreqTerms(uber_dtm, lowfreq=10), 40)
findAssocs(uber_dtm, 'cabify', 0.40)
Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
uber_dict  <- Dictionary(findFreqTerms(uber_dtm, 5))
head(uber_dict)
library(wordcloud)
wordcloud(uber_corpus_clean, random.order = FALSE, scale = c(3,0.5),
          max.words = 90,rot.per = 0.25, colors=brewer.pal(name = "Dark2", n = 8))
#min.freq = 1