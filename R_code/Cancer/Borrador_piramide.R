###############################03-04-2021####################################
library(munsell)
library(curl)
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(ggrepel)
library(gridExtra)

library(scales)
library(lubridate)
#############################################################################
cat("\f")
rm(list = ls())


setwd("~/Documents/Investigacion/Lina Javeriana/Actualización reciente/Documentos/")

demo <- read_excel("Cohorte.xlsx")
#table(demo$`grupo dx`)






#############################################################################
##########################Para cancer########################################
#############################################################################


demo22<-demo#General cancer#?????????????????????????????

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11para cancer.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1para cancer.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12para cancer.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111para cancer.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111para cancer.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111para cancer.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111para cancer.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_para cancer.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_para cancer.csv")






#############################################################################
##########################CAC Cérvix########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Cérvix" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Cérvix.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Cérvix.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Cérvix.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Cérvix.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Cérvix.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Cérvix.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Cérvix.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Cérvix.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Cérvix.csv")






#demo22<- filter(demo, `grupo dx`=="CAC Colorectal" )






#############################################################################
##########################CAC Colorectal########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Colorectal" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Colorectal.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Colorectal.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Colorectal.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Colorectal.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Colorectal.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Colorectal.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Colorectal.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Colorectal.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Colorectal.csv")










#demo22<- filter(demo, `grupo dx`=="CAC Estómago" )



#############################################################################
##########################CAC Estómago########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Estómago" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Estómago.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Estómago.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Estómago.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Estómago.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Estómago.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Estómago.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Estómago.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Estómago.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Estómago.csv")






#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Linfocitica Aguda" )



#############################################################################
##########################CAC Leucemia Linfocitica Aguda########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Leucemia Linfocitica Aguda" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Leucemia Linfocitica Aguda.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Leucemia Linfocitica Aguda.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Leucemia Linfocitica Aguda.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Leucemia Linfocitica Aguda.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Leucemia Linfocitica Aguda.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Leucemia Linfocitica Aguda.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Leucemia Linfocitica Aguda.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Leucemia Linfocitica Aguda.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Leucemia Linfocitica Aguda.csv")







#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Mielocitica Aguda" )




#############################################################################
##########################CAC Colorectal########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Colorectal" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Colorectal.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Colorectal.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Colorectal.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Colorectal.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Colorectal.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Colorectal.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Colorectal.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Colorectal.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Colorectal.csv")






#demo22<- filter(demo, `grupo dx`=="CAC Linfoma Hodgkin" )



#############################################################################
##########################CAC Linfoma Hodgkin########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Linfoma Hodgkin" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Linfoma Hodgkin.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Linfoma Hodgkin.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Linfoma Hodgkin.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Linfoma Hodgkin.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Linfoma Hodgkin.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Linfoma Hodgkin.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Linfoma Hodgkin.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Linfoma Hodgkin.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Linfoma Hodgkin.csv")




#demo22<- filter(demo, `grupo dx`=="CAC Linfoma No Hodgkin" )




#############################################################################
##########################CAC Linfoma No Hodgkin########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Linfoma No Hodgkin" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Linfoma No Hodgkin.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Linfoma No Hodgkin.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Linfoma No Hodgkin.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Linfoma No Hodgkin.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Linfoma No Hodgkin.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Linfoma No Hodgkin.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Linfoma No Hodgkin.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Linfoma No Hodgkin.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Linfoma No Hodgkin.csv")







#demo22<- filter(demo, `grupo dx`=="CAC Mama" )




#############################################################################
##########################CAC Mama########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Mama" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Mama.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Mama.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Mama.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Mama.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Mama.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Mama.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Mama.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Mama.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Mama.csv")





#demo22<- filter(demo, `grupo dx`=="CAC Melanoma" )





#############################################################################
##########################CAC Melanoma########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Melanoma" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Melanoma.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Melanoma.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Melanoma.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Melanoma.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Melanoma.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Melanoma.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Melanoma.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Melanoma.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Melanoma.csv")




#demo22<- filter(demo, `grupo dx`=="CAC Próstata" )




#############################################################################
##########################CAC Próstata########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Próstata" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Próstata.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Próstata.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Próstata.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Próstata.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Próstata.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Próstata.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Próstata.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Próstata.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Próstata.csv")





#demo22<- filter(demo, `grupo dx`=="CAC Pulmón" )




#############################################################################
##########################CAC Pulmón########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="CAC Pulmón" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11CAC Pulmón.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1CAC Pulmón.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12CAC Pulmón.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111CAC Pulmón.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111CAC Pulmón.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111CAC Pulmón.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111CAC Pulmón.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_CAC Pulmón.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_CAC Pulmón.csv")




#demo22<- filter(demo, `grupo dx`=="Glándulas tiroides y endocrinas" )




#############################################################################
##########################Glándulas tiroides y endocrinas########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Glándulas tiroides y endocrinas" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Glándulas tiroides y endocrinas.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Glándulas tiroides y endocrinas.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Glándulas tiroides y endocrinas.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Glándulas tiroides y endocrinas.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Glándulas tiroides y endocrinas.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Glándulas tiroides y endocrinas.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Glándulas tiroides y endocrinas.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Glándulas tiroides y endocrinas.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Glándulas tiroides y endocrinas.csv")






#demo22<- filter(demo, `grupo dx`=="Huesos y cartílagos articulares" )




#############################################################################
##########################Huesos y cartílagos articulares########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Huesos y cartílagos articulares" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Huesos y cartílagos articulares.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Huesos y cartílagos articulares.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Huesos y cartílagos articulares.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Huesos y cartílagos articulares.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Huesos y cartílagos articulares.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Huesos y cartílagos articulares.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Huesos y cartílagos articulares.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Huesos y cartílagos articulares.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Huesos y cartílagos articulares.csv")






#demo22<- filter(demo, `grupo dx`=="Labio, cavidad bucal y faringe" )



#############################################################################
##########################Labio, cavidad bucal y faringe########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Labio, cavidad bucal y faringe" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Labio, cavidad bucal y faringe.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Labio, cavidad bucal y faringe.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Labio, cavidad bucal y faringe.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Labio, cavidad bucal y faringe.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Labio, cavidad bucal y faringe.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Labio, cavidad bucal y faringe.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Labio, cavidad bucal y faringe.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Labio, cavidad bucal y faringe.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Labio, cavidad bucal y faringe.csv")





#demo22<- filter(demo, `grupo dx`=="Ojo, encéfalo, y otras partes del sistema nervioso" )



#############################################################################
##########################Ojo, encéfalo, y otras partes del sistema nervioso########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Ojo, encéfalo, y otras partes del sistema nervioso" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Ojo, encéfalo, y otras partes del sistema nervioso.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Ojo, encéfalo, y otras partes del sistema nervioso.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Ojo, encéfalo, y otras partes del sistema nervioso.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Ojo, encéfalo, y otras partes del sistema nervioso.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Ojo, encéfalo, y otras partes del sistema nervioso.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Ojo, encéfalo, y otras partes del sistema nervioso.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Ojo, encéfalo, y otras partes del sistema nervioso.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Ojo, encéfalo, y otras partes del sistema nervioso.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Ojo, encéfalo, y otras partes del sistema nervioso.csv")





#demo22<- filter(demo, `grupo dx`=="Otros órganos digestivos" )



#############################################################################
##########################Otros órganos digestivos########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros órganos digestivos" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros órganos digestivos.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros órganos digestivos.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros órganos digestivos.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros órganos digestivos.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros órganos digestivos.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros órganos digestivos.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros órganos digestivos.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros órganos digestivos.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros órganos digestivos.csv")





#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales femeninos" )




#############################################################################
##########################Otros órganos genitales femeninos########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros órganos genitales femeninos" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros órganos genitales femeninos.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros órganos genitales femeninos.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros órganos genitales femeninos.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros órganos genitales femeninos.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros órganos genitales femeninos.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros órganos genitales femeninos.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros órganos genitales femeninos.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros órganos genitales femeninos.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros órganos genitales femeninos.csv")





#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales masculinos" )



#############################################################################
##########################Otros órganos genitales masculinos########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros órganos genitales masculinos" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros órganos genitales masculinos.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros órganos genitales masculinos.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros órganos genitales masculinos.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros órganos genitales masculinos.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros órganos genitales masculinos.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros órganos genitales masculinos.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros órganos genitales masculinos.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros órganos genitales masculinos.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros órganos genitales masculinos.csv")




#demo22<- filter(demo, `grupo dx`=="Otros órganos respiratorios e intratorácicos" )



#############################################################################
##########################Otros órganos respiratorios e intratorácicos########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros órganos respiratorios e intratorácicos" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros órganos respiratorios e intratorácicos.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros órganos respiratorios e intratorácicos.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros órganos respiratorios e intratorácicos.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros órganos respiratorios e intratorácicos.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros órganos respiratorios e intratorácicos.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros órganos respiratorios e intratorácicos.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros órganos respiratorios e intratorácicos.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros órganos respiratorios e intratorácicos.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros órganos respiratorios e intratorácicos.csv")





#demo22<- filter(demo, `grupo dx`=="Otros sitios, sitios mal definidos, sitios no especificados" )



#############################################################################
##########################Otros sitios, sitios mal definidos, sitios no especificados########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros sitios, sitios mal definidos, sitios no especificados" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros sitios, sitios mal definidos, sitios no especificados.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros sitios, sitios mal definidos, sitios no especificados.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros sitios, sitios mal definidos, sitios no especificados.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros sitios, sitios mal definidos, sitios no especificados.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros sitios, sitios mal definidos, sitios no especificados.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros sitios, sitios mal definidos, sitios no especificados.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros sitios, sitios mal definidos, sitios no especificados.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros sitios, sitios mal definidos, sitios no especificados.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros sitios, sitios mal definidos, sitios no especificados.csv")





#demo22<- filter(demo, `grupo dx`=="Otros tumores de la piel" )



#############################################################################
##########################Otros tumores de la piel########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros tumores de la piel" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros tumores de la piel.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros tumores de la piel.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros tumores de la piel.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros tumores de la piel.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros tumores de la piel.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros tumores de la piel.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros tumores de la piel.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros tumores de la piel.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros tumores de la piel.csv")




#demo22<- filter(demo, `grupo dx`=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )



#############################################################################
##########################Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines.csv")






#demo22<- filter(demo, `grupo dx`=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )



#############################################################################
##########################Tejidos mesoteliales (excepto pulmón) y tejidos blandos########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Tejidos mesoteliales (excepto pulmón) y tejidos blandos.csv")




#demo22<- filter(demo, `grupo dx`=="Tumores secundarios" )


#############################################################################
##########################Tumores secundarios########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Tumores secundarios" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Tumores secundarios.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Tumores secundarios.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Tumores secundarios.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Tumores secundarios.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Tumores secundarios.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Tumores secundarios.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Tumores secundarios.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Tumores secundarios.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Tumores secundarios.csv")





#demo22<- filter(demo, `grupo dx`=="Vías urinarias" )



#############################################################################
##########################Vías urinarias########################################
#############################################################################
cat("\f")
rm(list = ls())

demo22<- filter(demo, `grupo dx`=="Vías urinarias" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
write.csv(c, file = "11Vías urinarias.csv")

#tabla de frecuancia por intervalos
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

Hombres<-table(age.cat(hombre$edad_anos, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$edad_anos, upper = 80, by = 5))

total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


cancer <- data.frame(total,edad)
write.csv(cancer, file = "1Vías urinarias.csv")


cancer2<- cancer %>% mutate(totH = sum(Hombres),
                            totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  # Nos quedamos con columnas utiles  
  select(edad, Sexo, `Poblacion por Sexo`) #%>%
#filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
write.csv(cancer2, file = "12Vías urinarias.csv")



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)
ff<-data_frame(f)
write.csv(ff, file = "111Vías urinarias.csv")

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )


##################################################
##################################################

colo3<-demo2 %>%
  count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(colo3, file = "1111Vías urinarias.csv")





prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
incidencia <- filter(sep_fecha3, mes!="02")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")
write.csv(final2, file = "11111Vías urinarias.csv")



ciudades<- data.frame(demo22[,c(19,20)])
Bogota <- filter(ciudades, cod_municip==11001)
Barranquilla <- filter(ciudades, cod_municip=="08001")
Bucaramanga <- filter(ciudades, cod_municip==68001)
Medellin <- filter(ciudades, cod_municip=="05001")
Cali <- filter(ciudades, cod_municip==76001)
#Tunja y cucuta aparte de BUcaramenga y Bogota compone centro oriente
Centro_oriente <- filter(ciudades, cod_municip==15001) #Tunja
Centro_oriente2 <- filter(ciudades, cod_municip==54001) #Cucuta
centro_fin<- rbind(Centro_oriente,Centro_oriente2)

Total<-rbind(Bogota,Barranquilla,Bucaramanga,Medellin,Cali,centro_fin)

division<-Total %>%
  count(municipio, sort = TRUE)

regional<- division %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2))
write.csv(regional, file = "111111Vías urinarias.csv")

#Descarga
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
write.csv(o, file = "1ciu_Vías urinarias.csv")

#Porcentaje de la población total
res2
write.csv(res2, file = "11ciu_Vías urinarias.csv")






