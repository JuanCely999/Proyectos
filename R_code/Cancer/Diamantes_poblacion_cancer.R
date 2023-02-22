##############################################################################
##################################Lina Cancer##
######################Autor: Juan Pablo Cely#################################
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

#demo22<-demo#General cancer
#demo22<- filter(demo, `grupo dx`=="CAC Cérvix" )
#demo22<- filter(demo, `grupo dx`=="CAC Colorectal" )
#demo22<- filter(demo, `grupo dx`=="CAC Estómago" )
#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Linfocitica Aguda" )
#demo22<- filter(demo, `grupo dx`=="CAC Leucemia Mielocitica Aguda" )
#demo22<- filter(demo, `grupo dx`=="CAC Linfoma Hodgkin" )
#demo22<- filter(demo, `grupo dx`=="CAC Linfoma No Hodgkin" )
#demo22<- filter(demo, `grupo dx`=="CAC Mama" )
#demo22<- filter(demo, `grupo dx`=="CAC Melanoma" )
#demo22<- filter(demo, `grupo dx`=="CAC Próstata" )
#demo22<- filter(demo, `grupo dx`=="CAC Pulmón" )
#demo22<- filter(demo, `grupo dx`=="Glándulas tiroides y endocrinas" )
#demo22<- filter(demo, `grupo dx`=="Huesos y cartílagos articulares" )
#demo22<- filter(demo, `grupo dx`=="Labio, cavidad bucal y faringe" )
#demo22<- filter(demo, `grupo dx`=="Ojo, encéfalo, y otras partes del sistema nervioso" )
#demo22<- filter(demo, `grupo dx`=="Otros órganos digestivos" )
#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales femeninos" )
#demo22<- filter(demo, `grupo dx`=="Otros órganos genitales masculinos" )
#demo22<- filter(demo, `grupo dx`=="Otros órganos respiratorios e intratorácicos" )
#demo22<- filter(demo, `grupo dx`=="Otros sitios, sitios mal definidos, sitios no especificados" )
#demo22<- filter(demo, `grupo dx`=="Otros tumores de la piel" )
#demo22<- filter(demo, `grupo dx`=="Otros tumores malignos del tejido linfático, de los órganos hematopoyéticos y de tejidos afines" )
#demo22<- filter(demo, `grupo dx`=="Tejidos mesoteliales (excepto pulmón) y tejidos blandos" )
demo22<- filter(demo, `grupo dx`=="Tumores secundarios" )
#demo22<- filter(demo, `grupo dx`=="Vías urinarias" )

demo2<- data.frame(demo22[,c(4,13,28)])

#Hombres
hombre <- filter(demo2, SEXO=="M")

#mujeres
mujer <- filter(demo2, SEXO=="F")

graf<-rbind(hombre,mujer)
b<-dim(graf)
c<-b[1]
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
#write.csv(alto73, file = "Absoluto73.csv")


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



#-------------------------------
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)

cancer2<- filter(cancer2, `Poblacion por Sexo` !=0 )
#cancer2<- filter(cancer2, Sexo =="Hombres" ) #Prostata
#cancer2<- filter(cancer2, Sexo =="Mujeres" ) #Cervix


p1<-ggplot(cancer2, aes(x = edad,
                       y = `Poblacion por Sexo`,
                       fill = Sexo)) +
  geom_bar(data = subset(cancer2, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(cancer2, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "red") +
  coord_flip() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +theme_light()+   
  #geom_text(aes(x=edad, y=`Poblacion por Sexo`, label = percent(`Poblacion por Sexo`/100), hjust = 2), position = position_dodge(width=0.9)) + 
  labs(title = "Distribución por género y grupo de edad",
       x = "",
       y = "Hombres                        Mujeres",caption = bquote("Edad promedio:" == .(f) ~ "años")) +theme(plot.caption = element_text(color = "red", size = 15,))+
  scale_y_continuous(breaks = seq(-30, 30, by = 2), labels = paste0(c(seq(-30, 0, by = 2)*-1, seq(2, 30, by = 2)), "%"))
  
#scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))
#scale_y_continuous(breaks = seq(-18, 18, by = 2), labels = paste0(c(seq(-18, 0, by = 2)*-1, seq(2, 18, by = 2)), "%"))

#scale_y_continuous(breaks = seq(-100, 100, by = 20), labels = paste0(c(seq(-100, 0, by = 20)*1, seq(20, 100, by = 20)), "%"))
d<-summary(demo2$edad_anos)
e<-d[4]
f<-round(e)

##################################################
##################################################

colo3<-demo2 %>%
    count(SEXO, sort = TRUE)

co2 <- na.omit(colo3)


colo3<- co2 %>% mutate(totH = sum(n)) %>%
  mutate(Porcentaje = (n/totH)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")

#colo3<- filter(colo3, SEXO =="M" ) #Prostata
#colo3<- filter(colo3, SEXO =="F" ) #Cervix

#--------------------------------
p2<-ggplot(colo3, aes(x=reorder(SEXO,n), y=n, fill=SEXO)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=SEXO, y=n, label = n, vjust=1), position = position_dodge(width=0.9)) + 
  geom_text(aes(x=SEXO, y=n, label = percent(Porcentaje/100), vjust=3), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("") +
  labs(title = "Distribución Tumores secundarios por genero",
       x = "",
       y = "", caption = bquote("Total:" == .(c) ~ "casos"))+theme(plot.caption = element_text(color = "red", size = 15,)) 
#aes(x=reorder(SEXO,n)

#plot.title = element_text(color = "red", size = 12, face = "bold"),
#plot.subtitle = element_text(color = "blue"),






##################Organizar fechas###################
prevale<- data.frame(demo22[,c(4:5)])

prevalencia <- na.omit(prevale)


sep_fecha <- separate(prevalencia, fecha_diagnostico, c("dia", "mes", "año"))

sep_fecha2 <- filter(sep_fecha, año==2020)
sep_fecha3 <- filter(sep_fecha2, mes!="01")
sep_fecha3 <- filter(sep_fecha3, mes!="02")
incidencia <- filter(sep_fecha3, mes!="03")


inci<-dim(incidencia)# Marzo 30 y diciembre 30 2020
inci2<-inci[1]

carac<-c("Prevalencia","Incidencia")

dat<-data_frame(rbind(c,inci2))

final<-cbind(carac,dat)
names(final)= c("Nombre", "Valor")

final2<- final %>% mutate(totf = sum(Valor)) %>%
  mutate(Porcentaje = (Valor/totf)*100) %>% mutate(Porcentaje=round(Porcentaje,2),"%")


p3<-ggplot(final2, aes(x=Nombre, y=Valor, fill=Nombre)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Nombre, y=Valor, label = Valor, vjust=1), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("") +
  labs(title = "Incidencia y prevalencia de Tumores secundarios",
       x = "",
       y = "", caption = "La incidencia de mide desde marzo a diciembre del 2020")+theme(plot.caption = element_text(color = "red", size = 15,)) 

grid.arrange(p2,p1,p3)
#700*800

##################Ciudades###################
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

#Descarga
#write.csv(regional, file = "Tumores secundarios_ciud.csv")
m<-sum(regional$n)
o<-m[1]


res<-o*100
res2<-res/c

#Numero de personas de las ciudades
o
#Porcentaje de la población total
res2


