##############################################################################
###Ejercicio: Tratamiento de datos y visualizaciones en piramides, terridata##
###############################Demografia 2019 y 1985########################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
rm(list=ls())
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


###Censos poblacionales https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/muestras-censales
#############################################################################
#####################Diagrama de 2019########################################
#####################Diagrama de Piramide de solo un municipio##############
#############################################################################

setwd("~/Documents/Investigacion/Alto ricaurte/edicion")


demo <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim25.xlsx (1)/TerriData_Dim25.xlsx")

demo2<- filter(demo, `Código Departamento`==15 )
demo2<- filter(demo2, `Código Entidad`!=15000 )
demo2<- filter(demo2, `Código Entidad`==15293)#GACHANTIVA
demo2 <- filter(demo2,`Año`==2019)
#Hombres
demo3 <- filter(demo2, `Unidad de Medida`=="Hombres")
demo3 <- filter(demo3, Indicador!="Población total de hombres")
demo4<- data.frame(demo3[,-c(5:6,9,11:12)])
demo4 = rename(demo4, Hombres="Dato.Numérico")
#Mujeres
demo5<- filter(demo2, `Unidad de Medida`=="Mujeres")
demo5 <- filter(demo5, Indicador!="Población total de mujeres")
demo5<- data.frame(demo5[,c(8)])
demo5 = rename(demo5, Mujeres="Dato.Numérico")
edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
             "20-24 años","25-29 años","30-34 años",
             "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
             "75-79 años","80+ años")
poblacion <- cbind(demo4,demo5,edad)
write.csv(poblacion, file = "poblacion.csv")
poblacion2 <- read_excel("poblacion2.xlsx")


#Transformar a numeros los caracteres
#poblacion$Hombres= as.factor(poblacion$Hombres)
#poblacion$Mujeres= as.factor(poblacion$Mujeres)
#poblacion$Hombres= as.numeric(poblacion$Hombres)
#poblacion$Mujeres= as.numeric(poblacion$Mujeres)
#D <- transform(d, fake_char = as.numeric(fake_char), 
              # char_fac = as.numeric(char_fac))


demografia<- poblacion2 %>% mutate(totH = sum(Hombres),
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
  select(Entidad, edad, Sexo, `Poblacion por Sexo`) #%>%
  #filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))
ggplot(demografia, aes(x = edad,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_bar(data = subset(demografia, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(demografia, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "Pirámide Poblacional de Gachantiva, 2019",
       x = "",
       y = "Hombres                        Mujeres",
       caption = "Fuente: Terridata 2019. Tabulados de Población \n Dane. Elaborado por Juan Pablo Cely") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))




#####################################################################################
######Diagrama de Piramide de alto ricaurte##########################################
#####################################################################################
gendemo2<- filter(demo, `Código Departamento`==15 )
gendemo2<- filter(gendemo2, `Código Entidad`!=15000 )
gendemo2 <- filter(gendemo2,`Año`==2019)
gendemo2 <- filter(gendemo2, Indicador!="Población total de hombres")
gendemo2 <- filter(gendemo2, Indicador!="Población total de mujeres")


############15293 GACHANTIVA############
gach<- filter(gendemo2, `Código Entidad`==15293)

#Hombres
gachh<- filter(gach, `Unidad de Medida`=="Hombres")
gachh<- data.frame(gachh[,c(8)])
#Mujeres
gachm<- filter(gach, `Unidad de Medida`=="Mujeres")
gachm<- data.frame(gachm[,c(8)])

############15808 TINJACÁ###############
tinj<- filter(gendemo2, `Código Entidad`==15808)

#Hombres
tinjh<- filter(tinj, `Unidad de Medida`=="Hombres")
tinjh<- data.frame(tinjh[,c(8)])
#Mujeres
tinjm<- filter(tinj, `Unidad de Medida`=="Mujeres")
tinjm<- data.frame(tinjm[,c(8)])


############15407 VILLA DE LEYVA###############
vill<- filter(gendemo2, `Código Entidad`==15407)

#Hombres
villh<- filter(vill, `Unidad de Medida`=="Hombres")
villh<- data.frame(villh[,c(8)])
#Mujeres
villm<- filter(vill, `Unidad de Medida`=="Mujeres")
villm<- data.frame(villm[,c(8)])

############15776 SUTAMARCHÁN ###############
suta<- filter(gendemo2, `Código Entidad`==15776)
#Hombres
sutah<- filter(suta, `Unidad de Medida`=="Hombres")
sutah<- data.frame(sutah[,c(8)])
#Mujeres
sutam<- filter(suta, `Unidad de Medida`=="Mujeres")
sutam<- data.frame(sutam[,c(8)])

############15696 SANTA SOFÍA ###############
sant<- filter(gendemo2, `Código Entidad`==15696)
#Hombres
santh<- filter(sant, `Unidad de Medida`=="Hombres")
santh<- data.frame(santh[,c(8)])
#Mujeres
santm<- filter(sant, `Unidad de Medida`=="Mujeres")
santm<- data.frame(santm[,c(8)])

############15600 RÁQUIRA###############
raqu<- filter(gendemo2, `Código Entidad`==15600)
#Hombres
raquh<- filter(raqu, `Unidad de Medida`=="Hombres")
raquh<- data.frame(raquh[,c(8)])
#Mujeres
raqum<- filter(raqu, `Unidad de Medida`=="Mujeres")
raqum<- data.frame(raqum[,c(8)])

############15638 SÁCHICA###############
sach<- filter(gendemo2, `Código Entidad`==15638)
#Hombres
sachh<- filter(sach, `Unidad de Medida`=="Hombres")
sachh<- data.frame(sachh[,c(8)])
#Mujeres
sachm<- filter(sach, `Unidad de Medida`=="Mujeres")
sachm<- data.frame(sachm[,c(8)])

alto<-cbind(gachh,tinjh,villh,sutah,santh,raquh,sachh,gachm,tinjm,villm,sutam,santm,raqum,sachm)
write.csv(alto, file = "alto.csv")




################################
#CORRER DESDE ACA 2019-------------------

#alto <- read_excel("piramide 2019.xlsx")
alto<- read_excel("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Demografia/piramide 2019.xlsx")

alto$Hombres = rowSums (alto[ , 2:8])
alto$Mujeres = rowSums (alto[ , 9:15])
alto<- data.frame(alto[,c(16,17)])
edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")
altor <- cbind(alto,edad)


names(alto)


piramide<- altor %>% mutate(totH = sum(Hombres),
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


a<-ggplot(piramide, aes(x = edad,
                       y = `Poblacion por Sexo`,
                       fill = Sexo)) +
  geom_bar(data = subset(piramide, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(piramide, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "2019",
       x = "",
       y = "Hombres                        Mujeres") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

    #   caption = "Fuente: Terridata 2019. Tabulados de Población \n Dane. Elaborado por Juan Pablo Cely"
write.csv(piramide, file = "piramide2019.csv")



#####################################################################################
##############################Diagrama del censo de 1985#############################
#####################################################################################
#489	=	Arcabuco, Villa de Leyva, Gachantivá, Santa Sofía, Sáchica
#491	=	Saboyá, San Miguel de Sema, 0, Tinjacá, Sutamarchán
#488	=	Cucaita, Motavita, Chiquiza, Sora, Ráquira, Samacá
rm(list=ls())
library(munsell)
library(curl)
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(haven)


##Censo 85 Descarcar nuevamente para mayor informacion
#censo_1985 <- read_sas("~/Documents/Investigacion/Alto ricaurte/MUESTRA_C85/MUESTRA_C85/archivo_sas/censo_1985.sas7bdat")
#1=hombre 2=mujer

names(altoR)#47age 48sex
altoR2<- data.frame(censo_1985[,c(10,47:48)])

write.csv(altoR2, file = "alto85inicial.csv")




#####################################1985---------------------------------
########correr desde aca
altoR<- read.csv("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Demografia/alto85inicial.csv")
altoR2<- filter(altoR, munico==489,491,488)


#Hombres
hombre<- filter(altoR2, sex==1)
#mujeres
mujer<- filter(altoR2, sex==2)


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

Hombres<-table(age.cat(hombre$age, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$age, upper = 80, by = 5))
total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
            "20-24 años","25-29 años","30-34 años",
            "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
            "75-79 años","80+ años")


alto85 <- data.frame(total,edad)
write.csv(alto85, file = "Absoluto85.csv")


piramide85<- alto85 %>% mutate(totH = sum(Hombres),
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


b<-ggplot(piramide85, aes(x = edad,
                     y = `Poblacion por Sexo`,
                     fill = Sexo)) +
  geom_bar(data = subset(piramide85, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(piramide85, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "1985",
       x = "",
       y = "Hombres                        Mujeres") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

write.csv(piramide85, file = "piramide85.csv")


















#####################################################################################
##############################Diagrama del censo de 1973 y 93 Descargas de censo#############################
#####################################################################################
#489	=	Arcabuco, Villa de Leyva, Gachantivá, Santa Sofía, Sáchica
#491	=	Saboyá, San Miguel de Sema, 0, Tinjacá, Sutamarchán
#488	=	Cucaita, Motavita, Chiquiza, Sora, Ráquira, Samacá
rm(list=ls())


##Censo 73 Descargar nuevamente para mayor informacion
censo_1973 <- read_sas("~/Documents/Investigacion/Alto ricaurte/MUESTRA_C73/MUESTRA_C73/archivo_sas/censo_1973.sas7bdat")
#1=hombre 2=mujer
names(censo_1973)
altoR2<- data.frame(censo_1973[,c(10,47:48)])

write.csv(altoR2, file = "alto73inicial.csv")

##Censo 93 Descargar nuevamente para mayor informacion
censo_1993 <- read_sas("~/Documents/Investigacion/Alto ricaurte/MUESTRA_C93/MUESTRA_C93/archivo_sas/censo_1993.sas7bdat")
#1=hombre 2=mujer
names(censo_1993)
altoR22<- data.frame(censo_1993[,c(10,47:48)])

write.csv(altoR22, file = "alto93inicial.csv")










#####################################1973---------------------------
########correr desde aca
altoR<- read.csv("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Demografia/alto73inicial.csv")
altoR2<- filter(altoR, munico==489,491,488)


#Hombres
hombre<- filter(altoR2, sex==1)
#mujeres
mujer<- filter(altoR2, sex==2)


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

Hombres<-table(age.cat(hombre$age, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$age, upper = 80, by = 5))
total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


alto73 <- data.frame(total,edad)
write.csv(alto73, file = "Absoluto73.csv")


piramide73<- alto73 %>% mutate(totH = sum(Hombres),
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


c<-ggplot(piramide73, aes(x = edad,
                       y = `Poblacion por Sexo`,
                       fill = Sexo)) +
  geom_bar(data = subset(piramide73, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(piramide73, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "1973",
       x = "",
       y = "Hombres                        Mujeres") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

write.csv(piramide73, file = "piramide73.csv")













#####################################1993
########correr desde aca
altoR<- read.csv("~/Documents/Investigacion/Alto ricaurte/Analisis alto ricaurte/Demografia/alto93inicial.csv")
altoR2<- filter(altoR, munico==489,491,488)


#Hombres
hombre<- filter(altoR2, sex==1)
#mujeres
mujer<- filter(altoR2, sex==2)


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

Hombres<-table(age.cat(hombre$age, upper = 80, by = 5))
Mujeres<-table(age.cat(mujer$age, upper = 80, by = 5))
total<-cbind(Hombres,Mujeres)

edad<-c("0-04 años","05-09 años","10-14 años","15-19 años",
        "20-24 años","25-29 años","30-34 años",
        "35-39 años","40-44 años","45-49 años","50-54 años","55-59 años","60-64 años","65-69 años","70-74 años",
        "75-79 años","80+ años")


alto93 <- data.frame(total,edad)
write.csv(alto93, file = "Absoluto93.csv")


piramide93<- alto93 %>% mutate(totH = sum(Hombres),
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


d<-ggplot(piramide93, aes(x = edad,
                       y = `Poblacion por Sexo`,
                       fill = Sexo)) +
  geom_bar(data = subset(piramide93, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(piramide93, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "",
       x = "",
       y = "Hombres                        Mujeres") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

write.csv(piramide93, file = "piramide93.csv")



grid.arrange(c,b,d,a)


