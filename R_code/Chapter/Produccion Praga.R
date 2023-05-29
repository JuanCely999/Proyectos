
#############################################################
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

rm(list=ls()[! ls() %in% c("S06A_Cultivos_")])



#S06A_Cultivos_ <- read_sav("~/Documents/Investigacion/Helmuth Praga/Total_nacional(spss)/Total_nacional(spss)/Total_nacional/S06A(Cultivos).sav")
#S01_15_Unidad_productora_ <- read_sav("/Volumes/Juan DD/Reducir almacenamiento/Helmuth Praga
S06A_Cultivos_ <- read_sav("/Volumes/Juan DD/Reducir almacenamiento/Helmuth Praga/Total_nacional(spss)/Total_nacional(spss)/Total_nacional/S06A(Cultivos).sav")




directorio <- "~/Documents/Investigacion/Mapas Pemp"
setwd(directorio)

colombia <-  st_read(dsn = "./MGN2021_MPIO_POLITICO/", layer = "MGN_MPIO_POLITICO")

Cultivos<- data.frame(S06A_Cultivos_[,c(4,10,19,20,22,23)])

#################General####################

Cultivos[is.na(Cultivos)] <- 0
cultivo2 <- Cultivos %>% group_by(P_MUNIC) %>% summarise(gen_can = sum(P_S6P57A))
cultivo3 <- Cultivos %>% group_by(P_MUNIC) %>% summarise(gen_sem = sum(AREA_SEMBRADA))
cultivo4 <- Cultivos %>% group_by(P_MUNIC) %>% summarise(gen_cos = sum(AREA_COSECHADA))
Cul<-Cultivos
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(gen_ren = mean(P_S6P59_UNIF))

general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
general2<-general %>% left_join(cultivo4, by="P_MUNIC")
general3<-general2 %>% left_join(cultivo5, by="P_MUNIC")


rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3")])


######################################Cafe
#001610
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,5)
cafe_cul<- filter(Cultivos, cod_alim==16100 )

cultivo2 <- cafe_cul %>% group_by(P_MUNIC) %>% summarise(caf_can = sum(P_S6P57A))
cultivo3 <- cafe_cul %>% group_by(P_MUNIC) %>% summarise(caf_sem = sum(AREA_SEMBRADA))
cultivo4 <- cafe_cul %>% group_by(P_MUNIC) %>% summarise(caf_cos = sum(AREA_COSECHADA))
Cul<-cafe_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(caf_ren = mean(P_S6P59_UNIF))

c_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
c_general2<-c_general %>% left_join(cultivo4, by="P_MUNIC")
c_general3<-c_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3","c_general3")])

######################################Aceite de palma
#001491
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
ace_cul<- filter(Cultivos, cod_alim=="001491" )

cultivo2 <- ace_cul %>% group_by(P_MUNIC) %>% summarise(ace_can = sum(P_S6P57A))
cultivo3 <- ace_cul %>% group_by(P_MUNIC) %>% summarise(ace_sem = sum(AREA_SEMBRADA))
cultivo4 <- ace_cul %>% group_by(P_MUNIC) %>% summarise(ace_cos = sum(AREA_COSECHADA))
Cul<-ace_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(ace_ren = mean(P_S6P59_UNIF))

ace_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
ace_general2<-ace_general %>% left_join(cultivo4, by="P_MUNIC")
ace_general3<-ace_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3","c_general3","ace_general3")])
#write.csv(ace_general3, file = "cultivo2.csv")

######################################Caña de azucar o panelera
#001802

Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
azu_cul<- filter(Cultivos, cod_alim=="001802" )



cultivo2 <- azu_cul %>% group_by(P_MUNIC) %>% summarise(azu_can = sum(P_S6P57A))
cultivo3 <- azu_cul %>% group_by(P_MUNIC) %>% summarise(azu_sem = sum(AREA_SEMBRADA))
cultivo4 <- azu_cul %>% group_by(P_MUNIC) %>% summarise(azu_cos = sum(AREA_COSECHADA))
Cul<-azu_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(azu_ren = mean(P_S6P59_UNIF))

azu_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
azu_general2<-azu_general %>% left_join(cultivo4, by="P_MUNIC")
azu_general3<-azu_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3","c_general3","ace_general3","azu_general3")])

######################################Cacao
#001640
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
caca_cul<- filter(Cultivos, cod_alim=="001640" )



cultivo2 <- caca_cul %>% group_by(P_MUNIC) %>% summarise(caca_can = sum(P_S6P57A))
cultivo3 <- caca_cul %>% group_by(P_MUNIC) %>% summarise(caca_sem = sum(AREA_SEMBRADA))
cultivo4 <- caca_cul %>% group_by(P_MUNIC) %>% summarise(caca_cos = sum(AREA_COSECHADA))
Cul<-caca_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(caca_ren = mean(P_S6P59_UNIF))

caca_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
caca_general2<-caca_general %>% left_join(cultivo4, by="P_MUNIC")
caca_general3<-caca_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3","c_general3","ace_general3","azu_general3","caca_general3")])


######################################Caucho AUN NO
#001950
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,5)
cafe_cul<- filter(Cultivos, cod_alim=="00195")





######################################ALGODÓN
#001921


Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,8)
alg_cul<- filter(Cultivos, cod_alim=="00192101")

cultivo2 <- alg_cul %>% group_by(P_MUNIC) %>% summarise(alg_can = sum(P_S6P57A))
cultivo3 <- alg_cul %>% group_by(P_MUNIC) %>% summarise(alg_sem = sum(AREA_SEMBRADA))
cultivo4 <- alg_cul %>% group_by(P_MUNIC) %>% summarise(alg_cos = sum(AREA_COSECHADA))
Cul<-alg_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(alg_ren = mean(P_S6P59_UNIF))

alg_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
alg_general2<-alg_general %>% left_join(cultivo4, by="P_MUNIC")
alg_general3<-alg_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3")])




######################################Tabaco AUN NO
#001970
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
cafe_cul<- filter(Cultivos, cod_alim=="001970")






######################################PLATANO
#001313
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
plat_cul<- filter(Cultivos, cod_alim=="001313")

cultivo2 <- plat_cul %>% group_by(P_MUNIC) %>% summarise(plat_can = sum(P_S6P57A))
cultivo3 <- plat_cul %>% group_by(P_MUNIC) %>% summarise(plat_sem = sum(AREA_SEMBRADA))
cultivo4 <- plat_cul %>% group_by(P_MUNIC) %>% summarise(plat_cos = sum(AREA_COSECHADA))
Cul<-plat_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(plat_ren = mean(P_S6P59_UNIF))

plat_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
plat_general2<-plat_general %>% left_join(cultivo4, by="P_MUNIC")
plat_general3<-plat_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3")])





######################################Papa
#001510
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
papa_cul<- filter(Cultivos, cod_alim=="001510")


cultivo2 <- papa_cul %>% group_by(P_MUNIC) %>% summarise(papa_can = sum(P_S6P57A))
cultivo3 <- papa_cul %>% group_by(P_MUNIC) %>% summarise(papa_sem = sum(AREA_SEMBRADA))
cultivo4 <- papa_cul %>% group_by(P_MUNIC) %>% summarise(papa_cos = sum(AREA_COSECHADA))
Cul<-papa_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(papa_ren = mean(P_S6P59_UNIF))

papa_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
papa_general2<-papa_general %>% left_join(cultivo4, by="P_MUNIC")
papa_general3<-papa_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3","papa_general3")])



######################################Yuca
#001592
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
yuca_cul<- filter(Cultivos, cod_alim=="001592")


cultivo2 <- yuca_cul %>% group_by(P_MUNIC) %>% summarise(yuca_can = sum(P_S6P57A))
cultivo3 <- yuca_cul %>% group_by(P_MUNIC) %>% summarise(yuca_sem = sum(AREA_SEMBRADA))
cultivo4 <- yuca_cul %>% group_by(P_MUNIC) %>% summarise(yuca_cos = sum(AREA_COSECHADA))
Cul<-yuca_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(yuca_ren = mean(P_S6P59_UNIF))

yuca_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
yuca_general2<-yuca_general %>% left_join(cultivo4, by="P_MUNIC")
yuca_general3<-yuca_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3","papa_general3","yuca_general3")])



######################################Banano
#001312
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
bana_cul<- filter(Cultivos, cod_alim=="001312")



cultivo2 <- bana_cul %>% group_by(P_MUNIC) %>% summarise(bana_can = sum(P_S6P57A))
cultivo3 <- bana_cul %>% group_by(P_MUNIC) %>% summarise(bana_sem = sum(AREA_SEMBRADA))
cultivo4 <- bana_cul %>% group_by(P_MUNIC) %>% summarise(bana_cos = sum(AREA_COSECHADA))
Cul<-bana_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(bana_ren = mean(P_S6P59_UNIF))

bana_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
bana_general2<-bana_general %>% left_join(cultivo4, by="P_MUNIC")
bana_general3<-bana_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3","papa_general3","yuca_general3","bana_general3")])

######################################Aguacate
#001311
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
agu_cul<- filter(Cultivos, cod_alim=="001311")


cultivo2 <- agu_cul %>% group_by(P_MUNIC) %>% summarise(agu_can = sum(P_S6P57A))
cultivo3 <- agu_cul %>% group_by(P_MUNIC) %>% summarise(agu_sem = sum(AREA_SEMBRADA))
cultivo4 <- agu_cul %>% group_by(P_MUNIC) %>% summarise(agu_cos = sum(AREA_COSECHADA))
Cul<-agu_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(agu_ren = mean(P_S6P59_UNIF))

agu_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
agu_general2<-agu_general %>% left_join(cultivo4, by="P_MUNIC")
agu_general3<-agu_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3","papa_general3","yuca_general3","bana_general3","agu_general3")])







######################################Arroz
#001132
Cultivos$cod_alim<-substring(Cultivos$P_S6P46, 0,6)
arr_cul<- filter(Cultivos, cod_alim=="001132")


cultivo2 <- arr_cul %>% group_by(P_MUNIC) %>% summarise(arr_can = sum(P_S6P57A))
cultivo3 <- arr_cul %>% group_by(P_MUNIC) %>% summarise(arr_sem = sum(AREA_SEMBRADA))
cultivo4 <- arr_cul %>% group_by(P_MUNIC) %>% summarise(arr_cos = sum(AREA_COSECHADA))
Cul<-arr_cul
Cultivos2<- filter(Cul, P_S6P59_UNIF !=0 )
cultivo5 <- Cultivos2 %>% group_by(P_MUNIC) %>% summarise(arr_ren = mean(P_S6P59_UNIF))

arr_general<-cultivo2 %>% left_join(cultivo3, by="P_MUNIC")
arr_general2<-arr_general %>% left_join(cultivo4, by="P_MUNIC")
arr_general3<-arr_general2 %>% left_join(cultivo5, by="P_MUNIC")

rm(list=ls()[! ls() %in% c("S06A_Cultivos_","Cultivos","general3",
                           "c_general3","ace_general3","azu_general3","caca_general3","alg_general3",
                           "plat_general3","papa_general3","yuca_general3","bana_general3","agu_general3", "arr_general3")])

#######################################################
#######################################################
colombia <-  st_read(dsn = "./MGN2021_MPIO_POLITICO/", layer = "MGN_MPIO_POLITICO")

colombia2 <- rename(colombia, c(P_MUNIC=MPIO_CDPMP))

colombia3<- data.frame(colombia2[,c(1:5,11:13)])
#colombia3<- data.frame(colombia2[,c(1,2)])



#colombia331 <- colombia3 %>% 
  #left_join(general3)

colombia331 <- general3 %>% 
  left_join(c_general3)

colombia331 <- colombia331 %>% 
  left_join(ace_general3)

colombia331 <- colombia331 %>% 
  left_join(azu_general3)

colombia331 <- colombia331 %>% 
  left_join(caca_general3)

colombia331 <- colombia331 %>% 
  left_join(alg_general3)

colombia331 <- colombia331 %>% 
  left_join(plat_general3)

colombia331 <- colombia331 %>% 
  left_join(papa_general3)

colombia331 <- colombia331 %>% 
  left_join(yuca_general3)

colombia331 <- colombia331 %>% 
  left_join(bana_general3)

colombia331 <- colombia331 %>% 
  left_join(agu_general3)

colombia331 <- colombia331 %>% 
  left_join(arr_general3)

write.csv(colombia331, file = "finalcultivo.csv")

#####################################

rm(list=ls()[! ls() %in% c("colombia331")])

colombia3<- data.frame(colombia331[,c(-8)])

colombia3<- data.frame(colombia331[,c(3,5,9:56)])


colombia3[is.na(colombia3)] <- 0


write.csv(colombia331, file = "finalcultivo.csv")
names(colombia331)
data_frame(colombia3)
data.frame(colombia3)

b1<- data.frame(colombia3[,c(1:200)])
b2<- data.frame(colombia3[,c(201:400)])
b3<- data.frame(colombia3[,c(401:600)])
b4<- data.frame(colombia3[,c(601:800)])
b5<- data.frame(colombia3[,c(801:1000)])
b6<- data.frame(colombia3[,c(1001:1200)])
b7<- data.frame(colombia3[,c(1201:1400)])
b8<- data.frame(colombia3[,c(1401:1600)])
b9<- data.frame(colombia3[,c(1601:1800)])
b10<- data.frame(colombia3[,c(1801:2000)])
b11<- data.frame(colombia3[,c(2001:2101)])


write.csv(b1, file = "b1.csv")
write.csv(b2, file = "b2.csv")
write.csv(b3, file = "b3.csv")
write.csv(b4, file = "b4.csv")
write.csv(b5, file = "b5.csv")
write.csv(b6, file = "b6.csv")
write.csv(b7, file = "b7.csv")
write.csv(b8, file = "b8.csv")
write.csv(b9, file = "b9.csv")
write.csv(b10, file = "b10.csv")
write.csv(b11, file = "b11.csv")













Cultivos$cod_alim <- as.numeric(Cultivos$cod_alim)


Cultivos33$cod_alim<-substring(Cultivos33$P_S6P46, 0,6)

names(Cultivos)
write.csv(cultivo2, file = "cultivo.csv")

#https://www.dane.gov.co/files/investigaciones/fichas/agropecuario/metodologia_CNA-01_V1.pdf
#https://www.dane.gov.co/files/sen/nomenclatura/cpc/CPC_tomoI_web.pdf
