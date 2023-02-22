##############################################################################
###Analisis de texto#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)        
tabu1<-read_xlsx("tabulacion1.xlsx")  #tabla abc                #se dividieron las tablas en documentos diferentes
tabu2<-read_xlsx("tabulacion2.xlsx") #tabla 012
names(tabu2)
class(tabu2)
summary(tabu2)
stem(tabu2$Cohesion)
hist(tabu2$Macrorreglas)
ggplot(tabu2,aes(x=Macrorreglas))+
  geom_density()+
  facet_wrap(~Codigo,scales="free")
tabu2%>% ggplot(., aes(x = Macrorreglas, y = Cohesion, color = Codigo)) +
  geom_line()
ggplot(tabu2, aes_(x = Cohesion, y = Macroreglas) + geom_point())
ggplot(data = tabu2,mapping = aes(x=Cohesion, y=Macrorreglas))



########################################################################################################
#######                                                                                        #########
#######                     ANALISIS DE TEXTO ENTREVISTAS                    #########
#######                                                                                        #########
########################################################################################################

# mas info en "http://www.gutenberg.org/ebooks/49836?msg=welcome_stranger"

#################################################
#setwd("C:/Users/emartigo/Desktop/Eduard Fernando Martinez Gonzalez/R")
setwd(('C:/Users/asus/Dropbox/TESIS MAESTRIA EN PEDAGOGIA'))
rm(list =ls()) 
#################################################


#SE NECESITAN LOS PAQUETES 
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("cluster")
install.packages("NLP")
install.packages("RColorBrewer")
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


#l parte

################################
acuerdos <- read_lines("docen.txt", skip = 1, n_max = 367-1 ) #135-1 entrevistas CD,218-1 entrevistas escritas leyendo el documento 135-0.txt desde la linea 1 hasta la linea 135
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
# Con nuestro Corpus mapeado de esta manera, podemos crear f?cilmente una nube de palabras (wordcloud de la librer?a del mismo nombre) que nos muestro los t?rminos m?s frecuentes en Niebla.
wordcloud(nov_ptd, max.words = 250, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))


########################        Mas depuraci?n      ########################

nov_text <- removeWords(nov_text, words = c("cada","leo","lee","pero","nadie","que", "una" , "los" , "por" , "del" , "muchos" , "tengo" , "cosas" , "mas" ,"muy" , "buena" , "las" , "los" , "sobre", "tener" , "pues", "todo", "sus","con","tambien","fue","mis","forma","una","porque","cuenta","gusta","por","como","para","mucho","acerca","son","nos","uno","como","que","asi","hacia","osea","hay","bueno","uno","como","que"))
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
  freq = nov_mat$frec, 
  max.words = 250, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8))

nov_mat[1:100,]
repetidas <- data.frame(nov_mat[1:100,])
#install.packages("xlsx")
#library(xlsx)
write_excel_csv(repetidas, "repetidas.csv")
# https://www.dropbox.com/s/16jom42nihraawf/Analisis_de_los_Nuevos_Acuerdos_de_Paz.R?dl=0

#ll parte

install.packages("tm")
library(tm)
DI=VCorpus(DirSource("C:/Users/asus/Dropbox/TESIS MAESTRIA EN PEDAGOGIA/DI.txt",encoding = "UTF-8"),readerControl = list(language="spa"))
inspect(DI)
DI=tm_map(DI,tolower) #diferenciar mayusculas y minusculas
DI=tm_map(DI,removePunctuation) #remueve signos e puntuacion
DI=tm_map(DI,removeWords,stopwords("spanish"))
DI=tm_map(DI,removeWords,c("laboral")) #remueve palabras muy frecuentes
install.packages("wordcloud")
library(wordcloud)
colores=brewer.pal(8,"Dark2")
wordcloud(DI[[1]],scale = c(3,0.5),
          max.words = 75,rot.per = 0.25,colors = colores)
wordcloud(DI[[2]],scale = c(3,0.5),
          max.words = 75,rot.per = 0.25,colors = colores)





