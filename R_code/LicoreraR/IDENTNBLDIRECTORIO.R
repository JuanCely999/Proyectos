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
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Enero/Enero/탍ea - Vivienda y Hogares.sav")



feb
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Febrero/Febrero/탍ea - Vivienda y Hogares.sav")

marz
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Marzo/Marzo/탍ea - Vivienda y Hogares.sav")

abr
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Abril/Abril/탍ea - Vivienda y Hogares.sav")

may
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Mayo/Mayo/탍ea - Vivienda y Hogares.sav")

jun
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Junio/Junio/탍ea - Vivienda y Hogares.sav")

juli
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Julio/Julio/탍ea - Vivienda y Hogares.sav")

agos
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Agosto/Agosto/탍ea - Vivienda y Hogares.sav")

sep
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Septiembre/Septiembre/탍ea - Vivienda y Hogares.sav")


oct
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Octubre/Octubre/탍ea - Vivienda y Hogares.sav")

nov
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Noviembre/Noviembre/탍ea - Vivienda y Hogares.sav")

dic
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2015/Diciembre/Diciembre/탍ea - Vivienda y Hogares.sav")









################################################################
################################################################
################################################################


#2016
ene
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Enero/Enero/탍ea - Vivienda y Hogares.sav")

feb
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Febrero/Febrero/탍ea - Vivienda y Hogares.sav")

marz
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Marzo/Marzo/탍ea - Vivienda y Hogares.sav")

abr
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Abril/Abril/탍ea - Vivienda y Hogares.sav")

may
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Mayo/Mayo/탍ea - Vivienda y Hogares.sav")

jun
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Junio/Junio/탍ea - Vivienda y Hogares.sav")

juli
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Julio/Julio/탍ea - Vivienda y Hogares.sav")

agos
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Agosto/Agosto/탍ea - Vivienda y Hogares.sav")

sep
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Septiembre/Septiembre/탍ea - Vivienda y Hogares.sav")


oct
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Octubre/Octubre/탍ea - Vivienda y Hogares.sav")

nov
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Noviembre/Noviembre/탍ea - Vivienda y Hogares.sav")

dic
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2016/Diciembre/Diciembre/탍ea - Vivienda y Hogares.sav")




################################################################
################################################################
################################################################


#2017
ene
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Enero/Enero/탍ea - Vivienda y Hogares.sav")

feb
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Febrero/Febrero/탍ea - Vivienda y Hogares.sav")

marz
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Marzo/Marzo/탍ea - Vivienda y Hogares.sav")

abr
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Abril/Abril/탍ea - Vivienda y Hogares.sav")

may
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Mayo/Mayo/탍ea - Vivienda y Hogares.sav")

jun
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Junio/Junio/탍ea - Vivienda y Hogares.sav")

juli
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Julio/Julio/탍ea - Vivienda y Hogares.sav")

agos
cab_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Cabecera - Vivienda y Hogares.sav")
resto_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/Resto - Vivienda y Hogares.sav")
area_viv<- read_sav("/Volumes/Juan DD/Licorera_boyaca/2017/Agosto/Agosto/햞ea - Datos del hogar y la vivienda.sav")

sep #
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Septiembre.dta/Septiembre.dta/area_viv.csv")


oct
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Octubre.dta/Octubre.dta/area_viv.csv")

nov
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/cabecera_viv.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/resto_viv.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Noviembre.dta/Noviembre.dta/area_viv.csv")

dic
cab_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2017/Diciembre.dta/Diciembre.dta/area.csv")





################################################################
################################################################
################################################################


#2018

#Mirar cuales funcionas con formato dta
ene
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Enero/area_viv.csv")

feb
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Febrero/area_viv.csv")

mar
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Marzo/area_viv.csv")

abr
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/Abril/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/Abril/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Abril/Abril/area_viv.csv")

may
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Mayo/area_viv.csv")

jun
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Junio/area_viv.csv")

jul
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Julio/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Julio/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Julio/Julio/탍ea - Vivienda y Hogares.dta")

ago
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Agosto/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Agosto/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Agosto/Agosto/탍ea - Vivienda y Hogares.dta")

sep
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Septiembre/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Septiembre/Septiembre/탍ea - Vivienda y Hogares.dta")

oct
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018Octubre/Octubre/cabecera_viv.csv")
resto_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018Octubre/Octubre/resto_viv.csv")
area_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018Octubre/Octubre/area_viv.csv")

nov
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Noviembre/Noviembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Noviembre/Noviembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Noviembre/Noviembre/탍ea - Vivienda y Hogares.dta")

dic
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2018/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Diciembre/Diciembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2018/Diciembre/Diciembre/탍ea - Vivienda y Hogares.dta")






################################################################
################################################################
################################################################


#2019

ene
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Enero/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Vivienda y Hogares.dta")

feb
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/area.csv")

mar
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Marzo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Vivienda y Hogares.dta")

abr
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Abril/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Vivienda y Hogares.dta")

may
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Mayo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Vivienda y Hogares.dta")

jun
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")

jul
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Vivienda y Hogares.dta")

ago
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Vivienda y Hogares.dta")

sep
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Vivienda y Hogares.dta")

oct
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")

nov
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")

dic
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Vivienda y Hogares.dta")





################################################################
################################################################
################################################################


#2020
Datos del hogar y la vivienda <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/1.Enero/1.Enero/DTA/Datos del hogar y la vivienda.DTA")

ene
#cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Enero/Cabecera - Vivienda y Hogares.dta")
#resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Vivienda y Hogares.dta")
#area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/1.Enero/1.Enero/DTA/Datos del hogar y la vivienda.DTA")


feb
#cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/Cabecera.csv")
#resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/resto.csv")
#area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/2.Febrero/2.Febrero/DTA/Datos del hogar y la vivienda.DTA")


mar
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Marzo/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/3.Marzo/3.Marzo/DTA/Datos del hogar y la vivienda.DTA")


abr
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Abril/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/4.Abril/4.Abril/DTA/Datos del hogar y la vivienda.DTA")


may
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Mayo/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/5.Mayo/5.Mayo/DTA/Datos del hogar y la vivienda.DTA")


jun
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/6.Junio/6.Junio/DTA/Datos del hogar y la vivienda.DTA")


jul
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Julio/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/7.Julio/7.Julio/DTA/Datos del hogar y la vivienda.DTA")


ago
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Agosto/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/8.Agosto/8.Agosto/DTA/Datos del hogar y la vivienda.DTA")

sep
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/9.Septiembre/9.Septiembre/DTA/Datos del hogar y la vivienda.DTA")

oct
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/10.Octubre/10.Octubre/DTA/Datos del hogar y la vivienda.DTA")

nov
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/11.Noviembre/11.Noviembre/DTA/Datos del hogar y la vivienda.DTA")

dic
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/12.Diciembre/12.Diciembre/DTA/Datos del hogar y la vivienda.DTA")




################################################################
################################################################
################################################################

################################################################
################################################################
################################################################


#2021

ene
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Enero/Enero/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Enero/Enero/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Enero/Enero/탍ea - Vivienda y Hogares.dta")

feb
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Febrero/Febrero/area.csv")

mar
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Marzo/Marzo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Marzo/Marzo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Marzo/Marzo/탍ea - Vivienda y Hogares.dta")

abr
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Abril/Abril/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Abril/Abril/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Abril/Abril/탍ea - Vivienda y Hogares.dta")

may
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Mayo/Mayo/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Mayo/Mayo/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Mayo/Mayo/탍ea - Vivienda y Hogares.dta")

jun
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Junio/Junio/area.csv")

jul
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Julio/Julio/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Julio/Julio/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Julio/Julio/탍ea - Vivienda y Hogares.dta")

ago
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Agosto/Agosto/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Agosto/Agosto/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Agosto/Agosto/탍ea - Vivienda y Hogares.dta")

sep
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Septiembre/Septiembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Septiembre/Septiembre/탍ea - Vivienda y Hogares.dta")

oct
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Octubre/Octubre/area.csv")

nov
cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/Cabecera.csv")
resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/resto.csv")
area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2021/Noviembre/Noviembre/area.csv")

dic
cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2021/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.dta")
resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Diciembre/Diciembre/Resto - Vivienda y Hogares.dta")
area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2021/Diciembre/Diciembre/탍ea - Vivienda y Hogares.dta")




################################################################
################################################################
################################################################


#2022
ene
#cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Enero/Enero/Cabecera - Vivienda y Hogares.dta")
#resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/Resto - Vivienda y Hogares.dta")
#area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Enero/Enero/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2022/GEIH_ENE_2022_MARCO_2018/GEIH_ENE_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.dta")


feb
#cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/Cabecera.csv")
#resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/resto.csv")
#area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Febrero/Febrero/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_FEB_2022_MARCO_2018/GEIH_FEB_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


mar
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Marzo/Marzo/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Marzo/Marzo/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_MARZO_2022_MARCO_2018/GEIH_MARZO_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


abr
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Abril/Abril/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Abril/Abril/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_ABRIL_2022_MARCO_2018/GEIH_ABRIL_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


may
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Mayo/Mayo/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Mayo/Mayo/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_mayo_2022_MARCO_2018/GEIH_mayo_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


jun
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Junio/Junio/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Junio_2022_MARCO_2018/GEIH_Junio_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


jul
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Julio/Julio/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Julio/Julio/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Junio_2022_MARCO_2018/GEIH_Junio_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")


ago
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Agosto/Agosto/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Agosto/Agosto/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_AGO_2022_MARCO_2018/GEIH_AGO_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")

sep
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Septiembre/Septiembre/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Septiembre/Septiembre/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Septiembre_2022_MARCO_2018/GEIH_Septiembre_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")

oct
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Octubre/Octubre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Octubre_2022_MARCO_2018/GEIH_Octubre_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")

nov
# cab_viv<-read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/Cabecera.csv")
# resto_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/resto.csv")
# area_viv<- read.csv("/Volumes/Juan DD/Licorera_boyaca/2019/Noviembre/Noviembre/area.csv")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Noviembre_2022_MARCO_2018/GEIH_Noviembre_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")

dic
# cab_viv<- read.csv("~/Documents/Investigacion/Licorera_Boyaca/Propuesta/Formato/2019/Diciembre/Diciembre/Cabecera - Vivienda y Hogares.dta")
# resto_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/Resto - Vivienda y Hogares.dta")
# area_viv<- read_dta("/Volumes/Juan DD/Licorera_boyaca/2019/Diciembre/Diciembre/탍ea - Vivienda y Hogares.dta")
Total <- read_dta("/Volumes/Juan DD/Licorera_boyaca/2020/GEIH_Diciembre_2022_MARCO_2018/GEIH_Diciembre_2022_MARCO_2018/DTA/Datos del hogar y la vivienda.DTA")




################################################################
################################################################
################################################################

################################################################
################################################################
################################################################
