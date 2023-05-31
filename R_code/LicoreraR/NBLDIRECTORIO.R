rm(list=ls())
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)



################################################################
################################################################
################################################################

#2015
ene
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/탍ea - Ocupados.sav")



feb
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/탍ea - Ocupados.sav")

marz
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/탍ea - Ocupados.sav")

abr
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/탍ea - Ocupados.sav")

may
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/탍ea - Ocupados.sav")

jun
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/탍ea - Ocupados.sav")

juli
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/탍ea - Ocupados.sav")

agos
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/탍ea - Ocupados.sav")

sep
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/탍ea - Ocupados.sav")

nov
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/탍ea - Ocupados.sav")









################################################################
################################################################
################################################################


#2016
ene
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/탍ea - Ocupados.sav")

feb
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/탍ea - Ocupados.sav")

marz
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/탍ea - Ocupados.sav")

abr
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/탍ea - Ocupados.sav")

may
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/탍ea - Ocupados.sav")

jun
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/탍ea - Ocupados.sav")

juli
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/탍ea - Ocupados.sav")

agos
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/탍ea - Ocupados.sav")

sep
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/탍ea - Ocupados.sav")

nov
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/탍ea - Ocupados.sav")




################################################################
################################################################
################################################################


#2017
ene
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/탍ea - Ocupados.sav")

feb
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/탍ea - Ocupados.sav")

marz
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/탍ea - Ocupados.sav")

abr
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/탍ea - Ocupados.sav")

may
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/탍ea - Ocupados.sav")

jun
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/탍ea - Ocupados.sav")

juli
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/탍ea - Ocupados.sav")

agos
cab<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/햞ea - Ocupados.sav")
###############################################
sep #
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/area.csv")

#.DTA
oct
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/area.csv")

nov
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/area.csv")

dic
cab<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/cabecera.csv")
resto <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/resto.csv")
area <- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/area.csv")





################################################################
################################################################
################################################################


#2018

#Mirar cuales funcionas con formato dta
ene
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/Cabecera.csv")
resto<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/resto.csv")
area<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/area.csv")

feb
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/area.csv")

mar
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/area.csv")

abr
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/area.csv")

may
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/area.csv")


jun
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/area.csv")

jul
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Julio/Julio/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Julio/Julio/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Julio/Julio/탍ea - Ocupados.dta")

ago
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Agosto/Agosto/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Agosto/Agosto/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Agosto/Agosto/탍ea - Ocupados.dta")

sep
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Septiembre/Septiembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Septiembre/Septiembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Septiembre/Septiembre/탍ea - Ocupados.dta")

oct
cab<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/Octubre/Cabecera.csv")
resto <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/Octubre/resto.csv")
area <- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Octubre/Octubre/area.csv")

nov
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Noviembre/Noviembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Noviembre/Noviembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Noviembre/Noviembre/탍ea - Ocupados.dta")

dic
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Diciembre/Diciembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Diciembre/Diciembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Diciembre/Diciembre/탍ea - Ocupados.dta")






################################################################
################################################################
################################################################


#2019

ene
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Ocupados.dta")

feb
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/area.csv")

mar
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Ocupados.dta")

abr
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Ocupados.dta")

may
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Ocupados.dta")

jun
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")

jul
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Ocupados.dta")

ago
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Ocupados.dta")

sep
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Ocupados.dta")

oct
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")

nov
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")

dic
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Ocupados.dta")





################################################################
################################################################
################################################################


#2020
Ocupados <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/1.Enero/1.Enero/DTA/Ocupados.DTA")

ene
#cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Cabecera - Ocupados.dta")
#resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Ocupados.dta")
#area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/1.Enero/1.Enero/DTA/Ocupados.DTA")


feb
#cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/Cabecera.csv")
#resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/resto.csv")
#area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/2.Febrero/2.Febrero/DTA/Ocupados.DTA")


mar
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/3.Marzo/3.Marzo/DTA/Ocupados.DTA")


abr
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/4.Abril/4.Abril/DTA/Ocupados.DTA")


may
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/5.Mayo/5.Mayo/DTA/Ocupados.DTA")


jun
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/6.Junio/6.Junio/DTA/Ocupados.DTA")


jul
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/7.Julio/7.Julio/DTA/Ocupados.DTA")


ago
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/8.Agosto/8.Agosto/DTA/Ocupados.DTA")

sep
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/9.Septiembre/9.Septiembre/DTA/Ocupados.DTA")

oct
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/10.Octubre/10.Octubre/DTA/Ocupados.DTA")

nov
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/11.Noviembre/11.Noviembre/DTA/Ocupados.DTA")

dic
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/12.Diciembre/12.Diciembre/DTA/Ocupados.DTA")




################################################################
################################################################
################################################################

################################################################
################################################################
################################################################


#2021

ene
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Enero/Enero/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Enero/Enero/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Enero/Enero/탍ea - Ocupados.dta")

feb
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/area.csv")

mar
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Marzo/Marzo/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Marzo/Marzo/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Marzo/Marzo/탍ea - Ocupados.dta")

abr
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Abril/Abril/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Abril/Abril/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Abril/Abril/탍ea - Ocupados.dta")

may
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Mayo/Mayo/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Mayo/Mayo/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Mayo/Mayo/탍ea - Ocupados.dta")

jun
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/area.csv")

jul
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Julio/Julio/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Julio/Julio/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Julio/Julio/탍ea - Ocupados.dta")

ago
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Agosto/Agosto/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Agosto/Agosto/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Agosto/Agosto/탍ea - Ocupados.dta")

sep
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Septiembre/Septiembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Septiembre/Septiembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Septiembre/Septiembre/탍ea - Ocupados.dta")

oct
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/area.csv")

nov
cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/Cabecera.csv")
resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/resto.csv")
area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/area.csv")

dic
cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Diciembre/Diciembre/Cabecera - Ocupados.dta")
resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Diciembre/Diciembre/Resto - Ocupados.dta")
area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Diciembre/Diciembre/탍ea - Ocupados.dta")




################################################################
################################################################
################################################################


#2022
ene
#cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Cabecera - Ocupados.dta")
#resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Ocupados.dta")
#area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2022/GEIH_ENE_2022_MARCO_2018/GEIH_ENE_2022_MARCO_2018/DTA/Ocupados.dta")


feb
#cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/Cabecera.csv")
#resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/resto.csv")
#area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_FEB_2022_MARCO_2018/GEIH_FEB_2022_MARCO_2018/DTA/Ocupados.DTA")


mar
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_MARZO_2022_MARCO_2018/GEIH_MARZO_2022_MARCO_2018/DTA/Ocupados.DTA")


abr
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_ABRIL_2022_MARCO_2018/GEIH_ABRIL_2022_MARCO_2018/DTA/Ocupados.DTA")


may
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_mayo_2022_MARCO_2018/GEIH_mayo_2022_MARCO_2018/DTA/Ocupados.DTA")


jun
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Junio_2022_MARCO_2018/GEIH_Junio_2022_MARCO_2018/DTA/Ocupados.DTA")


jul
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Junio_2022_MARCO_2018/GEIH_Junio_2022_MARCO_2018/DTA/Ocupados.DTA")


ago
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_AGO_2022_MARCO_2018/GEIH_AGO_2022_MARCO_2018/DTA/Ocupados.DTA")

sep
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Septiembre_2022_MARCO_2018/GEIH_Septiembre_2022_MARCO_2018/DTA/Ocupados.DTA")

oct
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Octubre_2022_MARCO_2018/GEIH_Octubre_2022_MARCO_2018/DTA/Ocupados.DTA")

nov
# cab<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
# resto<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
# area<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Noviembre_2022_MARCO_2018/GEIH_Noviembre_2022_MARCO_2018/DTA/Ocupados.DTA")

dic
# cab<-read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Cabecera - Ocupados.dta")
# resto<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Ocupados.dta")
# area<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Ocupados.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Diciembre_2022_MARCO_2018/GEIH_Diciembre_2022_MARCO_2018/DTA/Ocupados.DTA")




################################################################
################################################################
################################################################

################################################################
################################################################
################################################################
