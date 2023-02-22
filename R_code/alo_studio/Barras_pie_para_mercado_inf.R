#############################################################################
###Analisis de mercado: Proyecto VISOTA#####################################
######################Autor: Juan Pablo Cely#################################
###############################24-06-2020####################################
library(ggplot2)
library(reshape2)
library(readxl)


library(haven)
library(foreign)
library(readxl)
library(dplyr)
library(readxl)
library(tidyr)
library(paqueteadp)
library(tidyverse)
library(sf)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)
library(ggrepel)
library(haven)
library(readxl)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggspatial)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
m2 <- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/m2.xlsx")



########Se decide no tener en cuenta las graficas de torta por su cantidad
m2<-data_frame(m2)

m2 %>%
  group_by(Destino) %>%
  summarise(volume = sum(Nacional)) %>%
  mutate(share=volume/sum(volume)) %>%
  ungroup() %>% 
  arrange(desc(volume)) %>%
  mutate(Make=factor(Destino, levels = as.character(Destino))) %>% 
  ggplot(aes(x="", y= share, fill=Destino)) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()


Destino<-m2$Destino
Nacional<-m2$Nacional
  df <- data.frame(value = Nacional,
                 Group = Destino) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = Group),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 2,
         label = paste0(Group, " ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group, show.legend = FALSE)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()

###############################################################
#############################CORRECTO##################################
###############################################################

m2 <- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/m2.xlsx")


m2<- m2 %>% mutate(totH = sum(nacional)) %>%
  mutate(Nacional = (nacional/totH)*100)%>% mutate(totH2 = sum(boyaca)) %>%
  mutate(Boyaca = (boyaca/totH2)*100) 


Nacional<- data.frame(m2[,c(5)])
Boyaca<- data.frame(m2[,c(7)])
Destino<- data.frame(m2[,c(1)])


Porcentaje<-round(Nacional)
Porcentaje2<-round(Boyaca)


cegma1 = data.frame(Destino, Porcentaje, Porcentaje2)
cegma.long = melt(cegma1)

ggplot(cegma.long, aes(x=reorder(Destino,value), y=value, fill=variable)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Destino, y=value, label = percent(value/100), hjust=0), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") +coord_flip()
#https://rpubs.com/Rortizdu/140196


###########################################VIVIENDA 88 MUNICIPIOS NACIONAL
library(dplyr)
library(ggplot2)
library(ggmap) # for theme_nothing
df <- data.frame(value = c(85424, 89788),
                 Group = c("VIS", "No VIS")) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c("VIS", "No VIS")),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 2,
         label = paste0(Group, " ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group, show.legend = FALSE)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()

###########################################VIVIENDA 302 MUNICIPIOS NACIONAL

df <- data.frame(value = c(97474, 109179),
                 Group = c("VIS", "No VIS")) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c("VIS", "No VIS")),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 2,
         label = paste0(Group, " ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group, show.legend = FALSE)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()








###########################################VIVIENDA 88 MUNICIPIOS BOYACA

df <- data.frame(value = c(1322, 3751),
                 Group = c("VIS", "No VIS")) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c("VIS", "No VIS")),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 1,
         label = paste0(Group, " ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group, show.legend = FALSE)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()

###########################################VIVIENDA 302 MUNICIPIOS BOYACA

df <- data.frame(value = c(1881, 5436),
                 Group = c("VIS", "No VIS")) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c("VIS", "No VIS")),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 1,
         label = paste0(Group, " ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group, show.legend = FALSE)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()






###########################################TASAS DE RESIDUOS##########################
rm(list=ls())
tasas<- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/tasas.xlsx")
names(tasas2)
tasas2<-melt(tasas, id.vars = c("Indicador"))

tasas3 = rename(tasas2, c(Año=variable, Porcentaj=value))

tasas3<-tasas3%>%mutate(Porcentaje=round(Porcentaj,2))

ggplot(tasas3, aes(x=Año, y=Porcentaje, fill=Indicador)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Año, y=Porcentaje, label = Porcentaje, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") #+coord_flip() #reorder(Indicador,value)

###########################################RESIDUOS AL MEDIO AMBIENTE##########################
rm(list=ls())
resiper<- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/resiper.xlsx")
names(resiper)


ggplot(resiper, aes(x=Año, y=Residuos)) +  geom_bar(position="dodge", fill="green", stat="identity") +
  geom_text(aes(x=Año, y=Residuos, label = Residuos, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Toneladas") + theme_minimal() +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})  
###########################################RESIDUOS PER CAPITA Y POR HOGAR##########################
resiper2<-resiper
ggplot(tasas3, aes(x=Año, y=Porcentaje, fill=Indicador)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Año, y=Porcentaje, label = Porcentaje, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Porcentaje") #+coord_flip() #reorder(Indicador,value)

resiper3<- data.frame(resiper2[,-c(2)])
names(resiper3)
resiper3 <- rename(resiper3, c(`Per cápita`=Per.capita,`Por hogar`=Por.hogares))

resiper4<-melt(resiper3, id.vars = c("Año"))

resiper4 = rename(resiper4, c(`Residuos sólidos`=variable, Toneladas=value))

ggplot(resiper4, aes(x=Año, y=Toneladas, fill=`Residuos sólidos`)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Año, y=Toneladas, label = Toneladas, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Toneladas") #+coord_flip() #reorder(Indicador,value)

###########################################Oferta y demanda de recursos##########################
rm(list=ls())
oferdem<- read_excel("~/Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/oferdem.xlsx")
names(oferdem)
oferdem<-melt(oferdem, id.vars = c("Año"))
oferdem <- rename(oferdem, c(Balance=variable))

ggplot(oferdem, aes(x=Año, y=value, fill=Balance)) +  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(x=Año, y=value, label = value, vjust=-0.5), position = position_dodge(width=0.9)) + 
  theme_light()+theme(legend.position='bottom')+ xlab("") + ylab("Toneladas") #+coord_flip() #reorder(Indicador,value)



###########################################RESIDUOS EN LA MATRIZ##########################
rm(list=ls())
residuos<- read_excel("Documents/Investigacion/Analisis de mercado Villota/Mercado en R/Graficas de Torta/residuos.xlsx")
res1<- filter(residuos, Residuo=="Químicos sanitarios")

res11<-melt(res1, id.vars = c("Año","Residuo"))
res111<-res11%>%mutate(Toneladas=round(value))
#res111<- filter(res111, Toneladas !=0 )
res111 <- rename(res111, c(`Consumo intermedio y final`=variable))

a<-ggplot(res111, aes(x = Año, y = Toneladas, fill = `Consumo intermedio y final`, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Químicos sanitarios")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res2<- filter(residuos, Residuo=="Metálicos")

res21<-melt(res2, id.vars = c("Año","Residuo"))
res211<-res21%>%mutate(Toneladas=round(value))
#res211<- filter(res211, Toneladas !=0 )

b<-ggplot(res211, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Metálicos")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res3<- filter(residuos, Residuo=="No metálicos reciclables")

res31<-melt(res3, id.vars = c("Año","Residuo"))
res311<-res31%>%mutate(Toneladas=round(value))
#res311<- filter(res311, Toneladas !=0 )

c<-ggplot(res311, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "No metálicos reciclables")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res4<- filter(residuos, Residuo=="Vehículos y equipos descartados")

res41<-melt(res4, id.vars = c("Año","Residuo"))
res411<-res41%>%mutate(Toneladas=round(value))
#res411<- filter(res411, Toneladas !=0 )

d<-ggplot(res411, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Vehículos y equipos descartados")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


#########################
res5<- filter(residuos, Residuo=="Animales y vegetales" )

res51<-melt(res5, id.vars = c("Año","Residuo"))
res511<-res51%>%mutate(Toneladas=round(value))
#res511<- filter(res511, Toneladas !=0 )

e<-ggplot(res511, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Animales y vegetales")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res6<- filter(residuos, Residuo=="Mixtos y comerciales")

res61<-melt(res6, id.vars = c("Año","Residuo"))
res611<-res61%>%mutate(Toneladas=round(value))
#res611<- filter(res611, Toneladas !=0 )

f<-ggplot(res611, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Mixtos y comerciales")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


#########################
res7<- filter(residuos, Residuo=="Minerales y tierra")

res71<-melt(res7, id.vars = c("Año","Residuo"))
res711<-res71%>%mutate(Toneladas=round(value))
#res711<- filter(res711, Toneladas !=0 )

g<-ggplot(res711, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Minerales y tierra")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res8<- filter(residuos, Residuo=="De combustión")

res81<-melt(res8, id.vars = c("Año","Residuo"))
res811<-res81%>%mutate(Toneladas=round(value))
#res811<- filter(res811, Toneladas !=0 )

h<-ggplot(res811, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "De combustión")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 

#########################
res9<- filter(residuos, Residuo=="Otros")

res91<-melt(res9, id.vars = c("Año","Residuo"))
res911<-res91%>%mutate(Toneladas=round(value))
#res911<- filter(res911, Toneladas !=0 )

i<-ggplot(res911, aes(x = Año, y = Toneladas, fill = variable, label = Toneladas)) +
  geom_bar(stat = "identity",show.legend = FALSE) + labs(subtitle = "Otros")+ geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


#########################
res10<- filter(residuos, Residuo=="Radiactivos")

res101<-melt(res10, id.vars = c("Año","Residuo"))
res1011<-res101%>%mutate(Toneladas=round(value))
#res1011<- filter(res1011, Toneladas !=0 )
res1011 <- rename(res1011, c(`Consumo intermedio y final`=variable))

j<-ggplot(res1011, aes(x = Año, y = Toneladas, fill = `Consumo intermedio y final`, label = Toneladas)) +
  geom_bar(stat = "identity") + geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +theme_void()+ theme(legend.text=element_text(size=rel(1.2)))


grid.arrange(a,b,c,d,e,f,g,i,j)










#Tener encuenta en el futuro
library(ggplot2)
library(dplyr)

df <- data.frame(Make=c('toyota','toyota','honda','honda','jeep','jeep','jeep','accura','accura'),
                 Model=c('camry','corolla','city','accord','compass', 'wrangler','renegade','x1', 'x3'),
                 Cnt=c(10, 4, 8, 13, 3, 5, 1, 2, 1))
dfc <- df %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)*100.0) %>%
  arrange(desc(volume))

bp <- ggplot(dfc[c(1:10),], aes(x="", y= share, fill=Make)) +
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y")
pie



ggplot(dfc[1:10, ], aes("", share, fill = Make)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "market share") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))


df %>%
  group_by(Make) %>%
  summarise(volume = sum(Cnt)) %>%
  mutate(share=volume/sum(volume)) %>%
  ungroup() %>% 
  arrange(desc(volume)) %>%
  mutate(Make=factor(Make, levels = as.character(Make))) %>% 
  ggplot(aes(x="", y= share, fill=Make)) +
  geom_col() +
  geom_text(aes(label = scales::percent(round(share,3))), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void()




#https://stackoverflow.com/questions/41338757/adding-percentage-labels-on-pie-chart-in-r?rq=1
