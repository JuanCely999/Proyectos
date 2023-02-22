#########################################################################
###TALLER DE REFLEXI?N TE?RICA Y METODOL?GICA############################
#################"UNA HISTORIA POR HACER"###############################
##############Bibliometria y analisis textual###########################
#################Autor: Juan Pablo Cely#################################
#########################19-03-2020#####################################∫
#########################################################################

rm(list=ls())
library("dplyr")
library("Matrix")
library("stringr")
library("igraph")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("bibliometrix") ### load bibliometrix package
repositorio <- "/Users/asus/Documents/Clasificacion por tema/Trabajos/Curso Ciencias Sociales"
setwd(repositorio)
uptc <- readFiles("savedrecs (1).bib")
                  
               #   ,"savedrecs (2).bib")
                  
#                  ,"savedrecs (3).bib","savedrecs (4).bib"
 #                 ,"savedrecs (5).bib","savedrecs (6).bib")#sumatoria de los documentos recoopilados

UPTC <- convert2df(uptc, dbsource = "isi", format = "bibtex")
#results <- biblioAnalysis(UPTC, sep = ";")
#NetMatrix <- biblioNetwork(UPTC, analysis = "co-occurrences", network = "keywords", sep = ";")
M<-UPTC

###############Parte ll:An?lisis bibliom?trico#######################
##############################################
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:20])
results <- biblioAnalysis(M)
summary(results, k=10, pause=F, width=130)
win.graph()
plot(x=results, k=10, pause=F)
#CR <- citations(M, field = "article", sep = ";")
#cbind(CR$Cited[1:20])
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 10, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
#20
M=metaTagExtraction(M,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

histResults <- histNetwork(M, min.citations=quantile(M$TC,0.75), sep = ";")

options(width = 130)
#
net <- histPlot(histResults, n=20, size = 5, labelsize = 3)

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=3,label.cex=TRUE,label.n=30,edges.min=2)
#Anterior grafica tener encuenta
netstat <- networkStat(NetMatrix)
summary(netstat,k=10)
#
CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=100, clust=5, stemming=FALSE, labelsize=8,documents=20)
#cs=min Degree10
###IMPORTANTE
Map=thematicMap(M, field = "ID", n = 250, minfreq = 5,
                stemming = FALSE, size = 0.5, n.labels=3, repel = TRUE)
plot(Map$map)


############################################
###########################################

CS <- conceptualStructure(UPTC,field="ID", minDegree=20, k.max=21, stemming=FALSE)



M<-metaTagExtraction(UPTC, Field = "AU_CO", sep = ";")
NetMatrix<-biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 20, Title = "Country Collaboration", type = "circle", size=TRUE,remove.multiple=FALSE)
net=networkPlot(NetMatrix, n = 50, Title = "Country Collaboration", type = "fruchterman", size=FALSE, remove.multiple=TRUE, vos.path="C:/Users/Adriana.Diaz/Documents/Alonso/Observatorio/bibliometRics/VOSviewer",weighted=TRUE)
#Thematic map
#
##################################
###########No#####################
##################################
#################################
data(UPTC)
years=c(2011)
list_df=timeslice(UPTC, breaks = years)
M1=list_df[[1]]
M2=list_df[[2]]
NetMatrix1 <- biblioNetwork(M1, analysis = "co-occurrences",
                            network = "keywords", sep = ";")
S1 <- normalizeSimilarity(NetMatrix1, type = "association")
net1 <- networkPlot(NetMatrix1, normalize = "association",n = 50,
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res1 <- thematicMap(net1, NetMatrix1, S1)
plot(res1$map)
NetMatrix2 <- biblioNetwork(M2, analysis = "co-occurrences",
                            network = "keywords", sep = ";")
S2 <- normalizeSimilarity(NetMatrix2, type = "association")
net2 <- networkPlot(NetMatrix2, normalize = "association",n = 50,
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res2 <- thematicMap(net2, NetMatrix2, S2)
plot(res2$map)
nexus <- thematicEvolution(res1,res2,weighted=FALSE)



###############################################
####################################################
##########Parte lll: An?lisis textual###########
#http://www.aic.uva.es/cuentapalabras/analisis-de-sentimientos-aplicado-a-la-literatura.html#la-libreria-syuzhet
#Repositorio de los libros
#https://github.com/7PartidasDigital/AnalisisTextual/tree/master/textos 
library(tidyverse)
library(tidytext)
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
sentimientos
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")



trafalgar <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/01_EN-01-01-Trafalgar.txt",
                        locale = default_locale())
carlos4 <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/02_EN-01-02-La_Corte_de_Carlos_IV.txt",
                      locale = default_locale())
trafalgar <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/01_EN-01-01-Trafalgar.txt",
                        locale = default_locale())
marzo_mayo <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/03_EN-01-03-El_19_de_Marzo_y_el_2_de_Mayo.txt",
                         locale = default_locale())
bailen <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/04_EN-01-04-Bailen.txt",
                     locale = default_locale())
napoleon <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/05_EN-01-05-Napoleon_en_Chamartin.txt",
                       locale = default_locale())
zaragoza <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/06_EN-01-06-Zaragoza.txt",
                       locale = default_locale())
gerona <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/07_EN-01-07-Gerona.txt",
                     locale = default_locale())
cadiz <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/08_EN-01-08-Cadiz.txt",
                    locale = default_locale())
empecinado <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/09_EN-01-09-Juan_Martin_El_Empecinado.txt",
                         locale = default_locale())
arapiles <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/episodios/10_EN-01-10-La_Batalla_de_los_Arapiles.txt",
                       locale = default_locale())



titulos <- c("Trafalgar",
             "La Corte de Carlos IV",
             "El 19 de Marzo y el 2 de Mayo",
             "Bailen",
             "Napoleon en Chamartin",
             "Zaragoza",
             "Gerona",
             "Cadiz",
             "Juan Martin El Empecinado",
             "La Batalla de los Arapiles")


libros <- list(trafalgar,
               carlos4,
               marzo_mayo,
               bailen,
               napoleon,
               zaragoza,
               gerona,
               cadiz,
               empecinado,
               arapiles)

serie <- tibble()
for(i in seq_along(titulos)) {
  limpio <- tibble(capitulo = seq_along(libros[[i]]),
                   texto = libros[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(libro = titulos[i]) %>%
    select(libro, everything())
  serie <- rbind(serie, limpio)
}

serie$libro <- factor(serie$libro, levels = rev(titulos))

serie$palabra[1:50]

serie %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

serie %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo, libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")

recuenta_palabras_bing <- serie %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE)

recuenta_palabras_bing 

recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(25) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribuci?n al sentimiento", x = NULL) +
  coord_flip()





############################################
##############Parte lll: An?lisis textual###################
###########Segunda parte: Nube de palabras###############################
rm(list=ls())
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
#https://id.presidencia.gov.co/Paginas/prensa/2020/Declaracion-del-Presidente-Ivan-Duque-al-termino-del-Foro-Economico-Mundial-200123.aspx
acuerdos <- read_lines("dato.txt", skip = 1, n_max = 1500-1 ) #135-1 entrevistas CD,218-1 entrevistas escritas leyendo el documento 135-0.txt desde la linea 1 hasta la linea 135
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

########################DETENER######################
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
view(repetidas)
#install.packages("xlsx")
#library(xlsx)
#write_excel_csv(repetidas, "repetidas12.csv")
