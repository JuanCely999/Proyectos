library(rtweet)
library(tidyverse)
library(knitr)


setwd("~/Documents/Investigacion/Articulo Ligia /Analisis textual R/")



key<- "epb66XxlmRXoJMV9EVuKHrqLH"
secret<- "hjzbWFUCUtbdxFZmbqmDKNmL7ZN10u84GBErynalxxbtQymJbK"
appname<-"1012856937292226562-fCIjLw2xF0XYwk0c8GxgqVbrmOQzFw"
#access_token_secret<-"wR1gNompUCNIxbLmhu1E6p6ufHZRMJMwGcEftEUOWMpOg"
twitter_app         <- "TOURisgover"
twitter_token<- create_token(app = appname, consumer_key = key,
                           consumer_secret = secret)


extraccion_tweets <- function(usuario, maxtweets = 100, output_file_name = NULL){
  # Esta función extrae los tweets publicados por un usuario y los almacena en
  # un fichero csv. Si existe un fichero con el mismo nombre, lo lee, concatena
  # los nuevos tweets y lo sobrescribe.
  #
  # Argumentos:
  #   usuario: identificador del usuario de twitter
  #   maxtweets: número de tweets que se recuperan
  #   output_file_name: nombre del fichero de escritura
  
  # Si no se especifica el nombre del archivo de almacenamiento, se crea un
  # nombre por defecto
  if(is.null(output_file_name)){
    output_file_name <- paste0("datos_tweets_", usuario, ".csv")
  }
  
  # Si no existe el fichero de almacenamiento, se crea uno nuevo con los
  # resultados de la primera recuperación
  if(!(output_file_name %in% list.files())){
    datos_new <- get_timeline(user = usuario, n = maxtweets, parse = TRUE,
                              check = TRUE, include_rts = FALSE)
    write_csv(x = datos_new, path = output_file_name, col_names = TRUE)
    print("Nuevo fichero creado")
  }else{
    # Se leen los datos antiguos
    datos_old <- read_csv(file = output_file_name)
    # Se identifica el último Id recuperado
    ultimo_id <- tail(datos_old, 1)["status_id"] %>% pull()
    # Para no recuperar de nuevo el último tweet de la consulta anterior
    # se incrementa en 1 el Id
    ultimo_id = ultimo_id + 1
    # Para que no haya errores de compatibilidad, se convierten todas las
    # columnas numéricas a character
    datos_old <- map_if(.x = datos_old, .p = is.numeric, .f = as.character)
    # Extracción de nuevos tweets
    datos_new <- get_timeline(user = usuario, n = maxtweets, max_id = ultimo_id,
                              parse = TRUE, check = TRUE, include_rts = FALSE)
    datos_new <- map_if(.x = datos_new, .p = is.numeric, .f = as.character)
    # Concatenación de los datos nuevos, viejos y escritura en disco
    datos_concatenados <- bind_rows(datos_old, datos_new)
    write_csv(x = datos_concatenados, path = output_file_name, col_names = TRUE)
    print(paste("Número total de tweets:", nrow(datos_concatenados)))
    print(paste("Número de tweets nuevos:", nrow(datos_new)))
  }
}
extraccion_tweets(usuario  = "@elonmusk", maxtweets  = 200)

