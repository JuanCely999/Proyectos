##############################################################################
######################Ejercicio: Tratamiento de datos, terridata##############
################################Poblacion####################################
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
##############################################################################
##############################################################################
#########################Indices de pobreza#####################################
pobreza <- read_excel("~/Documents/Investigacion/Alto ricaurte/TerriData_Dim14.xlsx/TerriData_Dim14.xlsx")
indices<- filter(pobreza, `Código Departamento`==15 )

NBIDEP1<- filter(indices, `Código Entidad`==15000 )
NBIDEP2<- filter(indices, `Código Entidad`==15293)
NBIDEP3<- filter(indices, `Código Entidad`==15808)
NBIDEP4<- filter(indices, `Código Entidad`==15407)
NBIDEP5<- filter(indices, `Código Entidad`==15776)
NBIDEP6<- filter(indices, `Código Entidad`==15696)
NBIDEP7<- filter(indices, `Código Entidad`==15600)
NBIDEP8<- filter(indices, `Código Entidad`==15638)
NBIDEPt<-rbind(NBIDEP1,NBIDEP2,NBIDEP3,NBIDEP4,NBIDEP5,NBIDEP6,NBIDEP7,NBIDEP8)

NBIDEPt$Indicador
#"Índice de pobreza multidimensional - IPM"                                 
#[132] "IPM - Cabecera"                                                           
#[133] "IPM - Rural"                                                              
#[134] "Índice de Necesidades Básicas Insatisfechas - NBI - en el área urbana"    
#[135] "Índice de Necesidades Básicas Insatisfechas - NBI - en el área rural"     
#[136] "Población en condición de miseria"                                        
#[137] "Población en condición de miseria en el área urbana"                      
#[138] "Población en condición de miseria en el área rural" 

POB<- filter(NBIDEPt, Indicador== "Índice de pobreza multidimensional - IPM" )
POB2<- filter(NBIDEPt, Indicador=="IPM - Cabecera")
POB3<-filter(NBIDEPt, Indicador=="IPM - Rural")
POB4<-filter(NBIDEPt, Indicador=="Índice de Necesidades Básicas Insatisfechas - NBI - en el área urbana")
POB5<-filter(NBIDEPt, Indicador=="Índice de Necesidades Básicas Insatisfechas - NBI - en el área rural")
POB6<-filter(NBIDEPt, Indicador=="Población en condición de miseria")
POB7<-filter(NBIDEPt, Indicador=="Población en condición de miseria en el área urbana")
POB8<-filter(NBIDEPt, Indicador=="Población en condición de miseria en el área rural")
POBTOTAL<-rbind(POB,POB2,POB3,POB4,POB5,POB6,POB7,POB8)


write.csv(POBTOTAL, file = "POBTOTAL.csv")
