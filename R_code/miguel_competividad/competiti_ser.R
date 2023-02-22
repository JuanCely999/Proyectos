##############################################################################
###Analisis de competividad Miguel Castillo#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
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



#############################################3
########################Series de tiempo
cat("\f")
rm(list = ls())




#Crear fechas
meses <- c("01-01-13",
           "01-01-14",
           "01-01-15",
           "01-01-16",
           "01-01-17",
           "01-01-18",
           "01-01-19",
           "01-01-20")
years<- c("2013","2014", "2015", "2016", "2017", "2018", "2019", "2020")

meses <- data.frame(meses)

years <- data.frame(years)


ciclo5 <- read_excel("~/Documents/Investigacion/Miguel Castillo/datos r/graph.xlsx")

#################Antioquia

ciclo5$date <- as.Date(years$years, "%m-%d-%y")
ciclo5$year <- year(ciclo5$date)


ggplot(data = ciclo5, aes(x=date, y=valor, fill=Variables))+
  geom_line(aes(color = Variables), size = 1) + xlab("") + ylab("") +
  scale_color_manual(values = c("#FC4E07","#E7B800")) +
  theme_bw() +theme(legend.position='bottom') + scale_x_date(labels = date_format("%Y"), breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(.~variable)

date_trans()

