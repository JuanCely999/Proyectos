##############################################################################
###Turismo en Tunja###########################################
######################Autor: Juan Pablo Cely#################################
######################19/02/2020#################################
#Extracción de datos del DANE
library(foreign)
library(readxl)
library(ggplot2)  
library(readxl)
library(tidyr)
library(dplyr)
library(haven)
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
library(reshape2)

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
library(RColorBrewer)
library(fmsb)
cat("\f")
rm(list = ls())

##############################################################
#############################Arauca######################
##############################################################

setwd("~/Documents/Investigacion/Turismo Tunja/")

#Directorio
g191 <- read.csv2("~/Documents/Investigacion/Turismo Tunja/2019a/I.Trimestre/TURISMO/TURISMO.csv")
g192 <- read_dta("2019a/II.Trimestre/TURISMO/TURISMO.dta")
g193 <- read.csv2("~/Documents/Investigacion/Turismo Tunja/2019a/III.Trimestre/TURISMO/TURISMO.csv")
g194 <- read.csv2("~/Documents/Investigacion/Turismo Tunja/2019a/IV.Trimestre /TURISMO/TURISMO.csv")
g201 <- read_dta("2020a/I.Trimestre/TURISMO/TURISMO.dta")
g202 <- read.csv2("~/Documents/Investigacion/Turismo Tunja/2020a/II.Trimestre/TURISMO/TURISMO.csv")
g203 <- read.csv2("~/Documents/Investigacion/Turismo Tunja/2020a/III.Trimestre/TURISMO/TURISMO.csv")
g204 <- read_delim("2020a/IV.Trimestre/TURISMO/TURISMO.csv", 
                      +     "|", escape_double = FALSE, trim_ws = TRUE)


names(g201)
gc191<- data.frame(g191[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
"P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc191)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")


gc192<- data.frame(g192[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc192)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")


gc193<- data.frame(g193[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc193)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")



gc194<- data.frame(g194[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc194)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")


gc201<- data.frame(g201[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc201)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")

gc202<- data.frame(g202[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc202)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")

gc203<- data.frame(g203[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc203)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")

gc204<- data.frame(g204[,c("DIRECTORIO","FEX_C","P7570","P7571S1","P7571S1A3","P7580","P7580S1","P7573S1","P7573S2","P7574S1A1","P7574S5A1","P7574S2A1","P7574S8A1","P7574S4A1",
                           "P7574S10A1","P7574S3A1","P7574S7A2","P7575","P7579","P7581S1A1","P7581S10A1","P7581S11A1","P7581S3A1","P7581S4A1","P7581S5A1","P7581S6A1","P7581S7A1","P7581S8A1","P7581S9A1")])
names(gc204)= c("Directorio","Factor","a1","a2","a23","a3","a5","a6","a62","a7a2","a7b2","a7c2","a7d2","a7e3","a7f3","a7g3","a7h3","a8","a13","b1","b3","b10","b4","b5","b6","b7","b8","b9","b2")

total<-rbind(gc191,gc192,gc193,gc194,gc201,gc202,gc203,gc204)


rm(list=ls()[! ls() %in% c("total")])




table(total$a2)

fuera<- filter(total, a2==1)

#summary(fuera$a23)
fuera_cor<- filter(fuera, a23 >100)
summary(fuera_cor$a23)


fuera_cor2 <-fuera_cor %>% mutate(Factor2 = a23 * Factor) 
sum(fuera_cor2$Factor2)


###Numero de personas motivo de viaje factor expansion fuera del pais
fueraa31<- filter(fuera, a3==1)
sum(fueraa31$Factor)

fueraa32<- filter(fuera, a3==2)
sum(fueraa32$Factor)

fueraa33<- filter(fuera, a3==3)
sum(fueraa33$Factor)

fueraa34<- filter(fuera, a3==4)
sum(fueraa34$Factor)

fueraa35<- filter(fuera, a3==5)
sum(fueraa35$Factor)

fueraa36<- filter(fuera, a3==6)
sum(fueraa36$Factor)

fueraa37<- filter(fuera, a3==7)
sum(fueraa37$Factor)

fueraa38<- filter(fuera, a3==8)
sum(fueraa38$Factor)

fueraa39<- filter(fuera, a3==9)
sum(fueraa39$Factor)


###Numero de personas motivo de viaje factor expansion dentro del pais
dentro<- filter(total, a2==2)

dentroa31<- filter(dentro, a3==1)
sum(dentroa31$Factor)

dentroa32<- filter(dentro, a3==2)
sum(dentroa32$Factor)

dentroa33<- filter(dentro, a3==3)
sum(dentroa33$Factor)

dentroa34<- filter(dentro, a3==4)
sum(dentroa34$Factor)

dentroa35<- filter(dentro, a3==5)
sum(dentroa35$Factor)

dentroa36<- filter(dentro, a3==6)
sum(dentroa36$Factor)

dentroa37<- filter(dentro, a3==7)
sum(dentroa37$Factor)

dentroa38<- filter(dentro, a3==8)
sum(dentroa38$Factor)

dentroa39<- filter(dentro, a3==9)
sum(dentroa39$Factor)

sum(dentro$Factor)

###Numero de personas motivo de viaje factor expansion Boyaca
depar<- filter(total, a6==15)

depara31<- filter(depar, a3==1)
sum(depara31$Factor)

depara32<- filter(depar, a3==2)
sum(depara32$Factor)

depara33<- filter(depar, a3==3)
sum(depara33$Factor)

depara34<- filter(depar, a3==4)
sum(depara34$Factor)

depara35<- filter(depar, a3==5)
sum(depara35$Factor)

depara36<- filter(depar, a3==6)
sum(depara36$Factor)

depara37<- filter(depar, a3==7)
sum(depara37$Factor)

depara38<- filter(depar, a3==8)
sum(depara38$Factor)

depara39<- filter(depar, a3==9)
sum(depara39$Factor)


###Numero de personas motivo de viaje factor expansion Tunja
tunja<- filter(total, a62==15001)
#write.csv(tunja, file = "tunja.csv")

tunjaa31<- filter(tunja, a3==1)
sum(tunjaa31$Factor)

tunjaa32<- filter(tunja, a3==2)
sum(tunjaa32$Factor)

tunjaa33<- filter(tunja, a3==3)
sum(tunjaa33$Factor)

tunjaa34<- filter(tunja, a3==4)
sum(tunjaa34$Factor)

tunjaa35<- filter(tunja, a3==5)
sum(tunjaa35$Factor)

tunjaa36<- filter(tunja, a3==6)
sum(tunjaa36$Factor)

tunjaa37<- filter(tunja, a3==7)
sum(tunjaa37$Factor)

tunjaa38<- filter(tunja, a3==8)
sum(tunjaa38$Factor)

tunjaa39<- filter(tunja, a3==9)
sum(tunjaa39$Factor)

sum(tunja$Factor)



#El viaje lo realizo acompañado fuera del pais

fueraa51<- filter(fuera, a5==1)
sum(fueraa51$Factor)

fueraa52<- filter(fuera, a5==2)
sum(fueraa52$Factor)

fueraa53<- filter(fuera, a5==3)
sum(fueraa53$Factor)

fueraa54<- filter(fuera, a5==4)
sum(fueraa54$Factor)

fueraa55<- filter(fuera, a5==5)
sum(fueraa55$Factor)

#El viaje lo realizo acompañado dentro del pais
dentroa51<- filter(dentro, a5==1)
sum(dentroa51$Factor)

dentroa52<- filter(dentro, a5==2)
sum(dentroa52$Factor)

dentroa53<- filter(dentro, a5==3)
sum(dentroa53$Factor)

dentroa54<- filter(dentro, a5==4)
sum(dentroa54$Factor)

dentroa55<- filter(dentro, a5==5)
sum(dentroa35$Factor)


#El viaje lo realizo acompañado Boyaca
depar<- filter(total, a6==15)

depara51<- filter(depar, a5==1)
sum(depara51$Factor)

depara52<- filter(depar, a5==2)
sum(depara52$Factor)

depara53<- filter(depar, a5==3)
sum(depara53$Factor)

depara54<- filter(depar, a5==4)
sum(depara54$Factor)

depara55<- filter(depar, a5==5)
sum(depara55$Factor)



#El viaje lo realizo acompañado Boyaca
tunja<- filter(total, a62==15001)

tunjaa51<- filter(tunja, a5==1)
sum(tunjaa51$Factor)

tunjaa52<- filter(tunja, a5==2)
sum(tunjaa52$Factor)

tunjaa53<- filter(tunja, a5==3)
sum(tunjaa53$Factor)

tunjaa54<- filter(tunja, a5==4)
sum(tunjaa54$Factor)

tunjaa55<- filter(tunja, a5==5)
sum(tunjaa55$Factor)





####Noches en los lugares de residencia fuera del pais
table(fuera$a7a2)
table(fuera$a7b2)
table(fuera$a7c2)
table(fuera$a7d2)
table(fuera$a7e3)
table(fuera$a7f3)
table(fuera$a7g3)
table(fuera$a7h3)


####Noches en los lugares de residencia dentro del pais
table(dentro$a7a2)
table(dentro$a7b2)
table(dentro$a7c2)
table(dentro$a7d2)
table(dentro$a7e3)
table(dentro$a7f3)
table(dentro$a7g3)
table(dentro$a7h3)

####Noches en los lugares de residencia boyaca
table(depar$a7a2)
table(depar$a7b2)
table(depar$a7c2)
table(depar$a7d2)
table(depar$a7e3)
table(depar$a7f3)
table(depar$a7g3)
table(depar$a7h3)

####Noches en los lugares de residencia Tunja
table(tunja$a7a2)
table(tunja$a7b2)
table(tunja$a7c2)
table(tunja$a7d2)
table(tunja$a7e3)
table(tunja$a7f3)
table(tunja$a7g3)
table(tunja$a7h3)

#Medio de transporte
table(fuera$a8)
table(dentro$a8)
table(depar$a8)
table(tunja$a8)

#Motivos no viajar
table(fuera$a13)
table(dentro$a13)
table(depar$a13)
table(tunja$a13)






#########################Gastos fuera##################
fuera[is.na(fuera)] <- 0
fuera[fuera == 98] <- 0

#demo2<- na.omit(dentro$a23)
#dentro$a23 <- as.numeric(dentro$a23)
fuera_cos<-fuera %>% mutate(fuera_alojamiento = b1 * Factor) %>% 
  mutate(fuera_terre  = b3 * Factor) %>% 
  mutate(fuera_reven  = b10 * Factor) %>% 
  mutate(fuera_pub  = b4 * Factor) %>% 
  mutate(fuera_ali  = b5 * Factor) %>% 
  mutate(fuera_bien  = b6 * Factor) %>% 
  mutate(fuera_ser  = b7 * Factor) %>% 
  mutate(fuera_sou   = b8 * Factor) %>% 
  mutate(fuera_otro  = b9 * Factor) %>% 
  mutate(fuera_aer  = b2 * Factor)
  
sum(fuera_cos$fuera_alojamiento)
sum(fuera_cos$fuera_terre)
sum(fuera_cos$fuera_reven)
sum(fuera_cos$fuera_pub)
sum(fuera_cos$fuera_ali)
sum(fuera_cos$fuera_bien)
sum(fuera_cos$fuera_ser)
sum(fuera_cos$fuera_sou)
sum(fuera_cos$fuera_otro)
sum(fuera_cos$fuera_aer)



#########################Gastos dentro##################
dentro[is.na(dentro)] <- 0
dentro[dentro == 98] <- 0

dentro_cos<-dentro %>% mutate(dentro_alojamiento = b1 * Factor) %>% 
mutate(dentro_terre  = b3 * Factor) %>% 
  mutate(dentro_reven  = b10 * Factor) %>% 
  mutate(dentro_pub  = b4 * Factor) %>% 
  mutate(dentro_ali  = b5 * Factor) %>% 
  mutate(dentro_bien  = b6 * Factor) %>% 
  mutate(dentro_ser  = b7 * Factor) %>% 
  mutate(dentro_sou   = b8 * Factor) %>% 
  mutate(dentro_otro  = b9 * Factor) %>% 
  mutate(dentro_aer  = b2 * Factor)


sum(dentro_cos$dentro_alojamiento)
sum(dentro_cos$dentro_terre)
sum(dentro_cos$dentro_reven)
sum(dentro_cos$dentro_pub)
sum(dentro_cos$dentro_ali)
sum(dentro_cos$dentro_bien)
sum(dentro_cos$dentro_ser)
sum(dentro_cos$dentro_sou)
sum(dentro_cos$dentro_otro)
sum(dentro_cos$dentro_aer)





#########################Gastos Boyaca##################
depar[is.na(depar)] <- 0
depar[depar == 98] <- 0

depar_cos<-depar %>% mutate(depar_alojamiento = b1 * Factor) %>% 
  mutate(depar_terre  = b3 * Factor) %>% 
  mutate(depar_reven  = b10 * Factor) %>% 
  mutate(depar_pub  = b4 * Factor) %>% 
  mutate(depar_ali  = b5 * Factor) %>% 
  mutate(depar_bien  = b6 * Factor) %>% 
  mutate(depar_ser  = b7 * Factor) %>% 
  mutate(depar_sou   = b8 * Factor) %>% 
  mutate(depar_otro  = b9 * Factor) %>% 
  mutate(depar_aer  = b2 * Factor)


sum(depar_cos$depar_alojamiento)
sum(depar_cos$depar_terre)
sum(depar_cos$depar_reven)
sum(depar_cos$depar_pub)
sum(depar_cos$depar_ali)
sum(depar_cos$depar_bien)
sum(depar_cos$depar_ser)
sum(depar_cos$depar_sou)
sum(depar_cos$depar_otro)
sum(depar_cos$depar_aer)


#########################Gastos Tunja##################
tunja[is.na(tunja)] <- 0
tunja[tunja == 98] <- 0

tunja_cos<-tunja %>% mutate(tunja_alojamiento = b1 * Factor) %>% 
  mutate(tunja_terre  = b3 * Factor) %>% 
  mutate(tunja_reven  = b10 * Factor) %>% 
  mutate(tunja_pub  = b4 * Factor) %>% 
  mutate(tunja_ali  = b5 * Factor) %>% 
  mutate(tunja_bien  = b6 * Factor) %>% 
  mutate(tunja_ser  = b7 * Factor) %>% 
  mutate(tunja_sou   = b8 * Factor) %>% 
  mutate(tunja_otro  = b9 * Factor) %>% 
  mutate(tunja_aer  = b2 * Factor)


sum(tunja_cos$tunja_alojamiento)
sum(tunja_cos$tunja_terre)
sum(tunja_cos$tunja_reven)
sum(tunja_cos$tunja_pub)
sum(tunja_cos$tunja_ali)
sum(tunja_cos$tunja_bien)
sum(tunja_cos$tunja_ser)
sum(tunja_cos$tunja_sou)
sum(tunja_cos$tunja_otro)
sum(tunja_cos$tunja_aer)




















dentro<- filter(total, a2==2)

demo2<- na.omit(dentro$a23)
#dentro$a23 <- as.numeric(dentro$a23)

dentro_cor<- filter(dentro, a23 >100)
#summary(dentro_cor$a23)
table(dentro$a3)

sum(dentro$a3)

TotPesosCat <- colSums (Datos[ , 3:6])









gen19 <- read_dta("2020a/I.Trimestre/TURISMO/TURISMO.dta")#1

#gen20 <- read_dta("2020a/I.Trimestre/TURISMO/TURISMO.dta")#1
gen20<- read.csv2("2020a/II.Trimestre/TURISMO/TURISMO.csv")#2
#gen20 <- read.csv2("2020a/III.Trimestre/TURISMO/TURISMO.csv")#3
#gen20 <- read_delim("2020a/IV.Trimestre/TURISMO/TURISMO.csv", 
                      +     "|", escape_double = FALSE, trim_ws = TRUE)#4

#gen20 <- read_delim("2020a/IV.Trimestre/TURISMO/TURISMO.txt", 
                      +     "|", escape_double = FALSE, trim_ws = TRUE)
#gen20<-rbind(gen20a,gen20b,gen20c,gen20d)

##############################################################
#############################GENERAL######################
##############################################################
#a<-names(gen20)
gen20<-read.csv2("~/Documents/Investigacion/Turismo Tunja/2020a/II.Trimestre/TURISMO/TURISMO.csv")

names(esc)
data_frame(gen20)
esc<- filter(gen20, P7573S2=="05001")
gene<- data.frame(esc[,c(14)])

#demo2<- na.omit(gene)
#gene$P7580 <- as.numeric(gene$P7580)
table(esc$P7580)
table(esc$P7580S1)
table(esc$P7580S3)
table(esc$P7573S1)
table(esc$P7573S2)
table(esc$P7574S1)
table(esc$P7574S1A1)
table(esc$P7574S5)
table(esc$P7574S5A1)
table(esc$P7574S2)
table(esc$P7574S2A1)
table(esc$P7574S8)
table(esc$P7574S8A1)
table(esc$P7574S4)
table(esc$P7574S4A1)
table(esc$P7574S10)
table(esc$P7574S10A1)
table(esc$P7574S3)
table(esc$P7574S3A1)
table(esc$P7574S7)
table(esc$P7574S7A2)
table(esc$P7575)
table(esc$P7576)
table(esc$P7576S1)
table(esc$P7576S2)
table(esc$P7576S3)
table(esc$P7576S5)
table(esc$P7577S1)
table(esc$P7577S2)
table(esc$P7577S3)
table(esc$P7577S4)
table(esc$P7577S5)
table(esc$P7577S7)
table(esc$P7577S6)
table(esc$P7578)
table(esc$P7579)
table(esc$P7581S1)
table(esc$P7581S1A1)
table(esc$P7581S1A2)
table(esc$P7581S1A3)
table(esc$P7581S10)
table(esc$P7581S10A1)
table(esc$P7581S10A2)
table(esc$P7581S10A3)
table(esc$P7581S11)
table(esc$P7581S11A2)
table(esc$P7581S11A1)
table(esc$P7581S3)
table(esc$P7581S3A1)
table(esc$P7581S3A2)
table(esc$P7581S3A3)
table(esc$P7581S4)
table(esc$P7581S4A1)
table(esc$P7581S4A2)
table(esc$P7581S4A3)
table(esc$P7581S5)
table(esc$P7581S5A1)
table(esc$P7581S5A2)
table(esc$P7581S5A3)
table(esc$P7581S6)
table(esc$P7581S6A1)
table(esc$P7581S6A2)
table(esc$P7581S6A3)
table(esc$P7581S7)
table(esc$P7581S7A1)
table(esc$P7581S7A2)
table(esc$P7581S7A3)
table(esc$P7581S8)
table(esc$P7581S8A1)
table(esc$P7581S8A2)
table(esc$P7581S8A3)
table(esc$P7581S9)
table(esc$P7581S9A1)
table(esc$P7581S9A2)
table(esc$P7581S9A3)
table(esc$P549)
table(esc$P549S1)








cat("\f")
#rm(list = ls())

##################################################
tunja <- read_excel("Tunja valor.xlsx")


#total
p7578

#Alojamiento
p7581s1a1

#Alojamiento2
p7581s1a3

#transporte terrestre
p7581s10a1

#transporte terrestre2
p7581s10a3

#Gastos en bienes de la reventa
p7581s11a1

#transporte del lugar visitado
p7581s3a1

#transporte del lugar visitado2
p7581s3a3

#Alimentos y bebidas
p7581s4a1

#Alimentos y bebidas2
p7581s4a3

#Bienes de uso personal
p7581s5a1

#Servicios culturales y recreacionales
p7581s6a1

p7581s6a3

#souveniers, artesanias y regalos
p7581s7a1


#Otros gastos relacionados con el viaje
p7581s8a1

#Transporte aéreo (hacia y desde el destino)
p7581s9a1


names(tunja)

a1<- data.frame(tunja[,c("p7578","p7581s1a1","p7581s10a1","p7581s11a1","p7581s3a1","p7581s4a1","p7581s5a1","p7581s6a1","p7581s7a1","p7581s8a1","p7581s9a1")])
names(a1)= c("Total","Alojamiento", "Transporte terrestre","Gastos en bienes de la reventa","Transporte del lugar visitado", "Alimentos y bebidas","Bienes de uso personal", "Servicios culturales y recreacionales","souveniers, artesanias y regalos","Otros gastos", "Transporte aéreo")

names(a1)


co3<-melt(a1)

co4 <- filter(co3, value>100)
co5 <- filter(co4, value<1000000)

#Diagrama de cajas
ggplot(data = co3, aes(x = variable, y = value)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = variable), color = 'black', alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('') + 
  ylab('Millones de pesos') + guides(fill=FALSE)  +
  theme_minimal() + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ theme(axis.text.x = element_text(angle = 90))


summary(a1$`Transporte del lugar visitado`)


