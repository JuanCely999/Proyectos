##############################################################################
###Desagregacion bases de datos ingresos laborales desde GEIH NLB##
#http://microdatos.dane.gov.co/index.php/catalog/207/get_microdata
######################Autor: Juan Pablo Cely#################################
###############################15-03-2023####################################
#rm(list=ls())
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)
library(gridExtra)# Para organizar múltiples gráficos juntos
library(tidyverse)
library(ggversa)
#P4030s1a1
#Otro formato de CSV

# total4 <- read_csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total.csv", dec=".")
# total5 <- read_csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total2.csv")
# total6 <- read_csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total3.csv", dec=".")
rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","final2022")])
deflactor <- read_excel("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/deflactor.xlsx")
total1 <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total.csv")
#total2 <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total2.csv")
#total3 <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/total3.csv")
#recuperacion <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/Total/recuperacion2017.csv")
total1<- data.frame(total1[,c(2:10)]) 
#fin<-rbind(final2018,final2019,final2020,final2021,final2022)

full<-rbind(total1,final2018,final2019,final2020,final2021,final2022)


exp2<-as.numeric(full$exp)

concatenado <- paste(full$ano,full$mes, sep = "")
full2<-data.frame(full,concatenado,exp2)
summary(full2$exp2)

#table(full2$estrato)

full2$estrato<-fct_recode(full2$estrato,"1"="Bajo - bajo ", "2"="Bajo", 
                          "3"="Medio - bajo","4"="Medio","5"="Medio - alto",
                          "9"="No sabe o cuenta con planta el\xe9ctrica",
                          "0"="Conexi\xf3n pirata","30"="marzo","40"="abril",
                          "50"="mayo","60"="junio","70"="julio","9"="Inf")


table(full2$estrato)

bd_combinada <- merge(full2, deflactor, by = "concatenado")

full_det<- bd_combinada %>% mutate(in_def = ingreso / deflactor)

full_clean<- filter(full_det, ingreso !=0 )
full_clean2<- filter(full_clean, ingreso !="Inf")

full_cero<- filter(full_det, ingreso ==0 )
table(full_cero$estrato)
table(full_cero$fecha)
dim(full_cero)

table(full_clean$estrato)
table(full_clean$concatenado)
dim(full_clean)
full_clean2$mes<-fct_recode(full_clean2$mes, "1"="01","2"="02","3"= "03","4"="04","5"="05","6"="06","7"="07","8"="08","9"="09")


#########Promedio de ingresos segun estrato para calcular meses faltantes
#div <- full_clean2  %>% group_by(estrato) %>% summarise(estra = mean(in_def))
meses_falta<- filter(full_clean2, estrato == 30 )
meses_falta2<- filter(full_clean2, estrato == 40 )
meses_falta3<- filter(full_clean2, estrato == 50 )
meses_falta4<- filter(full_clean2, estrato == 60 )
meses_falta5<- filter(full_clean2, estrato == 70 )
m_fal<-rbind(meses_falta,meses_falta2,meses_falta3,meses_falta4,
             meses_falta5)
m_fal$in_def <- recode(m_fal$estrato, "1:645000=1; 645001=1000000=2")

m_fal$estrato <- case_when(
  m_fal$in_def <= 830000 ~ "1",
  m_fal$in_def > 830001 & m_fal$in_def <= 1350000 ~ "2",
  m_fal$in_def > 1350000 & m_fal$in_def <= 2340000 ~ "3",
  m_fal$in_def > 2340000 & m_fal$in_def <= 2600000 ~ "4",
  m_fal$in_def > 2600001 ~ "5"
)

#full_clean3<-rbind(full_clean2,m_fal)

#########################################################################

rm(list=ls()[! ls() %in% c("final2018","final2019","final2020","final2021","final2022","full_clean2")])

#Parte importante para comenzar


#est1<- filter(full_clean2, estrato ==4 )
#est1<- filter(full_clean2, name == "cab" )
#est1<- filter(full_clean2, name == "resto" )
est1<- full_clean2

#est1$mes<-fct_recode(est1$mes, "1"="01","2"="02","3"= "03","4"="04","5"="05","6"="06","7"="07","8"="08","9"="09")
est12 <- est1 %>% group_by(fecha) %>% summarise(mes2 = mean(in_def))

est13 <- merge(est1, est12, by = "fecha")


r<-summary(est13$in_def) #eliminar outliers
Mean<- r[4]
Q1<- r[2]
Q3<- r[5]
IQR<- Q3-Q1
ati<- Q3 + 2.5 * IQR #Se define como valor at?pico leve aquel que dista 1,5 veces el rango intercuant?lico por debajo de Q1 o por encima de Q3
ati2<- Q1 - 2.5* IQR 
coo <- filter(est13, in_def<ati)
CNANA <- filter(coo, in_def>ati2)

CN_SIN <- CNANA %>% group_by(fecha) %>% summarise(mes2 = mean(in_def))



#est1$in_def <- recode(est1$in_def, "in_def:ati"="r[4]")

#; 4:6=2; 7=3; 8=4"


###oTRO GRAFICO
# violinplot3 <- ggplot(CNANA, aes(y=in_def, x=factor(ano)))
# violinplot3 +
#   geom_violin(aes(fill=(factor(mes))),
#               draw_quantiles = c(0.25, 0.5, 0.75))+
#   annotate("text", x=2,y= 20, size=8,
#            label="")+
#   labs(x="Año", y="OH")+
#   theme(axis.title=element_text(size=10,face="bold"))+
#   ylab("Ingresos laborales")

#vio4+stat_summary(aes(group = in_def), fun.y = "mean", geom = "line", size = 1.5, color = "red") 


####GRAFICO OFICIAL

ggplot(data = CNANA, aes(x=ano, y=in_def, fill=mes, color=mes2))+
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = in_def), color = 'black', alpha = 0.8, show.legend = FALSE) +
  xlab("Año") + ylab("Ingresos laborales reales ($)") +
  geom_boxplot(color = 'black', alpha = 0.7)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) 


#####GRAFICO CON EL FACTOR EXPANSION
CNANA2 <- CNANA %>%
  uncount(weights = CNANA$exp2)

# ggplot(data = CNANA2, aes(x=ano, y=in_def, fill=mes, color=mes2))+
#   geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
#   geom_violin(aes(fill = in_def), color = 'black', alpha = 0.8, show.legend = FALSE) +
#   xlab("Año") + ylab("Ingresos laborales reales ($)") +
#   geom_boxplot(color = 'black', alpha = 0.7)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

CN12 <- CNANA2  %>% group_by(fecha) %>% summarise(mes2 = mean(in_def))

#valores iniciales; sin atipicos, con expansion
estrato1<-cbind(est12,CN_SIN,CN12)

#write.csv(estrato1, file = "estrato1.csv")
#write.csv(estrato1, file = "estrato2.csv")
 #write.csv(estrato1, file = "estrato3.csv")
#write.csv(estrato1, file = "estrato4.csv")
#write.csv(estrato1, file = "cabecera.csv")
#write.csv(estrato1, file = "resto.csv")
write.csv(estrato1, file = "total.csv")



summary(est1$in_def)
summary(CNANA$in_def)
summary(CNANA2$in_def)












# Crear dataframe con información de ingresos diarios
datos_ingresos <- data.frame(fecha = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day"),
                             ingreso = runif(1096, min = 1000, max = 50000))

# Agregar columna con el mes correspondiente
datos_ingresos$mes <- format(datos_ingresos$fecha, "%Y-%m")

# Agrupar ingresos por mes y calcular la mediana
datos_mes <- datos_ingresos %>%
  group_by(mes) %>%
  summarise(mediana_ingresos = median(ingreso))

# Crear gráfico de violines
ggplot(datos_mes, aes(x = mes, y = mediana_ingresos)) +
  geom_violin() +
  xlab("Mes") +
  ylab("Mediana de ingresos") +
  ggtitle("Distribución de ingresos por mes")


#"1"="Bajo - bajo", 

#full<-rbind(total1,total2,total3)

#final2018$estrato<-fct_recode(final2018$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ","1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")
fullexp_na <- subset(full2, is.na(exp2) )
fullestrato<- subset(full2, is.na(estrato) )

table(fullexp_na$concatenado)
table(fullestrato$concat)

cabv <- na.omit(full2)
table(cabv$concatenado)
#suma de filas

#revision numero faltante
sum(is.na(full2$exp))


#full$estrato<-fct_recode(full$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ","1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")


# ed<-as.numeric(total1$exp)
# op<-cbind(total1,ed)
# class(total3$exp)
#Cabecera[is.na(Cabecera)] <- 0
#cabecera2<- total3 %>% mutate(in_lab = exp * ingreso)
#cab <- cabecera2 %>% group_by(DPTO) %>% summarise(ing_lab = sum(in_lab))
class(full2$exp)
#revisar rango
summary(full2$exp)
#tabular
table(full$exp)
#crear base de datos faltante
total2exp_na <- subset(total2, is.na(exp) )
#concatenar


full2$estrato<-fct_recode(full2$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

full2$estrato<-fct_recode(full2$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ",
                          "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto","1"="1", "2"="2", "3"="3","4"="4","5"="5")


table(full$estrato)


fullexp_na <- subset(full2, is.na(exp) )
fullestrato<- subset(full2, is.na(estrato) )

table(fullexp_na$concatenado)
table(fullestrato$concatenado)

table(full2$estrato)


full2$estrato<-fct_recode(full2$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")
full2$estrato<-fct_recode(full2$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ",
                          "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto","1"="1", "2"="2", "3"="3","4"="4","5"="5")


full3<-filter(full2, concatenado==20188 )
f_estrato<- data.frame(full3[,c(3,11)])  
names(full2)

table(f_estrato$estrato)

#######################################
rep <- paste(recuperacion$ano,recuperacion$mes, sep = "")
rep2<-data.frame(recuperacion,rep)

repexp_na <- subset(rep2, is.na(exp) )
repestrato<- subset(rep2, is.na(estrato) )
table(repestrato$rep)





a20179$estrato<-fct_recode(a20179$estrato, "1"="Bajo - bajo", "1"="Bajo - bajo ", "2"="Bajo", "3"="Medio - bajo","4"="Medio","5"="Medio - alto")

table(a20179$estrato)













rm(list=ls()[! ls() %in% c("final2015","final2016","final2017","final2018","final2019","final2020","final2021","final2022")])

total <- rbind(final2015,final2016,final2017,final2018,final2019,final2020,final2021,final2022)

rm(list=ls()[! ls() %in% c("total")])



 












table(total$estrato)

table(total1$estrato)
df_nuevo <- head(resto2, n = 2)
#multiplicacion de datos por si mismo
df_nuevo2 <- df_nuevo %>%
  uncount(weights = df_nuevo$Fex_c_2011)
table(df_nuevo2$Fex_c_2011)

# Mostrar el nuevo data frame
df_nuevo



####Formatos ejemplo violin
ggplot(data = paitot, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('') + 
  ylab('$') + guides(fill=FALSE)   +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

#contrastado con las series de tiempo normal

ggplot(data = colombia3, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1) + xlab("") + ylab("Porcentaje") +
  scale_color_manual(values = c("green4","orangered","blue")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  facet_wrap(.~Departamento)

##ejemplo complementado
ggplot(data = colombia3, aes(x=date, y=valor, fill=Variables))+
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

























Resto_Ocupados2<- data.frame(Resto_Ocupados[,c(1,151,153:154)])

resto <- na.omit(Resto_Ocupados2)
sum(is.na(datos))



columna1_sin_na <- Resto_Ocupados2$Inglabo[!is.na(Resto_Ocupados2$Inglabo)]



datos <- Resto_Ocupados2[!is.na(Resto_Ocupados2$Inglabo),]
datos2 <- datos[!is.na(datos$Directorio),]


datos2$Inglabo <- format(datos2$Inglabo, scientific = FALSE)


write.csv(datos2, "Documents/Investigacion/Licorera_Boyaca/Bases_de_datos/datos.csv", row.names = FALSE)

names(Resto_Ocupados)


#install.packages("RMySQL")
#library(RMySQL)

install.packages("RODBC")
library(RODBC)

con <- odbcConnect("localhost", uid = "sa", pwd = "MiPassw0rd!1521")

############################################

######IMPORTANTE
setwd("~/Documents/Investigacion/Licorera_Boyaca/SQLlicorera/")
cat("\f")
rm(list = ls())
hola1 <- read_excel(paste("Libro2.xlsx",sep=""),1)
hola2 <- read_excel(paste("Libro2.xlsx",sep=""),2)
hola3 <- read_excel(paste("Libro2.xlsx",sep=""),3)
hola4 <- read_excel(paste("Libro2.xlsx",sep=""),4)
hola5 <- read_excel(paste("Libro2.xlsx",sep=""),5)


bd_combinada <- merge(hola1, hola2, by = "a")
bd_combinada$Variable_resultado <- bd_combinada$b * bd_combinada$c


concatenado <- paste(hola3$b,hola3$a, sep = "")

total<- data.frame(hola4,concatenado)



ggplot(data = total, aes(x=concatenado, y=d, fill=c))+
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = c), color = 'black', alpha = 0.8, show.legend = FALSE) +
  stat_summary(aes(group = c), fun.y = "mean", geom = "line", size = 1.5, color = "red") +
  geom_boxplot(color = 'black', alpha = 0.7)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




df_nuevo2 <- hola5 %>%
  uncount(weights = hola5$f)
table(hola5$f)


# concatenado <- data.frame(concatenado)
# 
# 
# hola4$date <- as.Date(concatenado$concatenado, format = "%m-%Y")
# hola4$year <- year(hola4$date)
# 
# 
# 
# EMM_agregada2$date <- as.Date(meses$meses, "%m-%d-%y")
# EMM_agregada2$year <- year(EMM_agregada2$date)


# 
# concatenado2 <- data.frame(concatenado,hola4)
# 
# 
# concatenado2$concatenado <- as.Date(concatenado2$concatenado, format = "%m-%Y")
# serie_tiempo <- ts(concatenado2$mi_variable, start = c(year(concatenado2$concatenado[1]), month(concatenado2$concatenado[1])), frequency = 12)

###############################################


#complemento segun chat gpt
# Importa los datos
#datos <- read.csv("datos.csv")





#hola4$date <- format(as.Date(concatenado$concatenado), "%d/%m/%Y")
#hola4$date <- as.Date(concatenado$concatenado, "%m-%d-%y")
#hola4$year <- year(hola4$date)



#concatenado2$concatenado <- as.Date(concatenado2$concatenado)

# Agrupa los datos por mes
#datos_mes <- aggregate(d ~ format(concatenado, "%Y-%m"), concatenado2, mean)

# Crea el diagrama de violín
install.packages("vioplot")
library(sm)
violinplot(concatenado2$d, plot.type = "single", col = "blue")

# Agrega ejes y título
axis(1, at = 1:length(datos_mes$valor), labels = datos_mes$`format(fecha, "%Y-%m")`, tick = FALSE)
axis(2)
title("Diagrama de violín de la serie de tiempo mensual")









# Importa los datos
datos <- read.csv("datos.csv")

concatenado <- paste(hola3$a, hola3$b, sep = "-")

# 
# concatenado <- data.frame(concatenado)

datos$fecha <- as.Date(datos$fecha)

# Agrupa los datos por mes
datos_mes <- aggregate(valor ~ format(fecha, "%Y-%m"), datos, mean)

# Crea el diagrama de violín
install.packages("vioplot")
library(vioplot)
violinplot(datos_mes$valor, plot.type = "single", col = "blue")

# Agrega ejes y título
axis(1, at = 1:length(datos_mes$valor), labels = datos_mes$`format(fecha, "%Y-%m")`, tick = FALSE)
axis(2)
title("Diagrama de violín de la serie de tiempo mensual")
