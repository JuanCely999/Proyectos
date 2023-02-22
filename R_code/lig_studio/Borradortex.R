red_social <- read_excel("red_social.xlsx")
red <- paste(red_social)
palabras<- tokenize_words(red)
palabras
length(palabras)
tabla <- table(palabras[[1]])
tabla <- data_frame(palabra = names(tabla), recuento = as.numeric(tabla))
arrange(tabla, desc(recuento))

oraciones <- tokenize_sentences(red)

oraciones_palabras <- tokenize_words(oraciones[[1]])

length(oraciones_palabras)

sapply(oraciones_palabras, length)



entrevista <- read_excel("entrevista.xlsx")
ent <- paste(entrevista)
palabras2<- tokenize_words(ent)






sentimientos <- readr::read_tsv("https://tinyurl.com/SentiEsp",
                                col_types = "cccn",
                                locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")



titulos <- c("Red",
             "Ent")

libros <- list(palabras,palabras2)

serie <- tibble()

for(i in seq_along(titulos)) {
  limpio <- tibble(capitulo = seq_along(libros[[i]]),
                   texto = libros[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(libro = titulos[i]) %>%
    select(libro, everything())
  serie <- rbind(palabra, limpio)
}

serie$palabra[1:50]






















nov_mat = data_frame(nov_mat)
names(nov_mat)<-c("pal","fre")


nov_mat  %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)


nov_mat %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

#austen_bigrams <- austen_books() %>%
#  unnest_tokens(bigram, text, token = "ngrams", n = 2)
