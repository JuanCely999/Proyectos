rm(list=ls())
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)
#2009
setwd("~/Documents/Tesis/Bases de datos/")
Enero

Cabecera <- read_sav("Ingresos laboral/2009/Enero/01/Cabecera - Ocupados (1).sav")
resto <- read_sav("Ingresos laboral/2009/Enero/01/Resto - Ocupados (1).sav")
area<- read_sav("Ingresos laboral/2009/Enero/01/탍ea - Ocupados  (1).sav")
febrero
Cabecera<- read_sav("Ingresos laboral/2009/Febrero/02/Cabecera - Ocupados (2).sav")
resto<- read_sav("Ingresos laboral/2009/Febrero/02/Resto - Ocupados (2).sav")
area<- read_sav("Ingresos laboral/2009/Febrero/02/탍ea - Ocupados (2).sav")

marzo

Cabecera <- read_sav("Ingresos laboral/2009/Marzo/03/Cabecera - Ocupados (3).sav")
resto <- read_sav("Ingresos laboral/2009/Marzo/03/Resto - Ocupados (3).sav")
area<- read_sav("Ingresos laboral/2009/Marzo/03/탍ea - Ocupados (3).sav")


Abril
Cabecera<- read_sav("Ingresos laboral/2009/04/04/Cabecera - Ocupados (4).sav")
resto<- read_sav("Ingresos laboral/2009/04/04/Resto - Ocupados (4).sav")
area<- read_sav("Ingresos laboral/2009/04/04/Área - Ocupados (4).sav")

Mayo
Cabecera<- read_sav("Ingresos laboral/2009/05/05/Cabecera - Ocupados (5).sav")
resto<- read_sav("Ingresos laboral/2009/05/05/Resto - Ocupados (5).sav")
area<- read_sav("Ingresos laboral/2009/05/05/Área - Ocupados (5).sav")

Junio
Cabecera<- read_sav("Ingresos laboral/2009/06/06/Cabecera - Ocupados (6).sav")
resto<- read_sav("Ingresos laboral/2009/06/06/Resto - Ocupados (6).sav")
area<- read_sav("Ingresos laboral/2009/06/06/Área - Ocupados (6).sav")

Julio
Cabecera<- read_sav("Ingresos laboral/2009/07/07/Cabecera - Ocupados (7(.sav")
resto<- read_sav("Ingresos laboral/2009/07/07/Resto - Ocupados (7).sav")
area<- read_sav("Ingresos laboral/2009/07/07/Área - Ocupados (7).sav")
                                                              
Agosto
Cabecera<- read_sav("Ingresos laboral/2009/08/08/Cabecera - Ocupados (8).sav")
resto<- read_sav("Ingresos laboral/2009/08/08/Resto - Ocupados (8).sav")
area<- read_sav("Ingresos laboral/2009/08/08/Área - Ocupados (8).sav")

sep
Cabecera<- read_sav("Ingresos laboral/2009/09/09/Cabecera - Ocupados (9).sav")
resto<- read_sav("Ingresos laboral/2009/09/09/Resto - Ocupados (9).sav")
area<- read_sav("Ingresos laboral/2009/09/09/Área - Ocupados (9).sav")

oct
Cabecera<- read_sav("Ingresos laboral/2009/10/10/Cabecera - Ocupados (10).sav")
resto<- read_sav("Ingresos laboral/2009/10/10/Resto - Ocupados (10).sav")
area<- read_sav("Ingresos laboral/2009/10/10/Área - Ocupados (10).sav")

nov
Cabecera<- read_sav("Ingresos laboral/2009/11/11/Cabecera - Ocupados (11).sav")
resto<- read_sav("Ingresos laboral/2009/11/11/Resto - Ocupados (11).sav")
area<- read_sav("Ingresos laboral/2009/11/11/Área - Ocupados (11).sav")

dic
Cabecera<- read_sav("Ingresos laboral/2009/12/12/Cabecera - Ocupados (12).sav")
resto<- read_sav("Ingresos laboral/2009/12/12/Resto - Ocupados (12).sav")
area<- read_sav("Ingresos laboral/2009/12/12/Área - Ocupados (12).sav")


################################################################
################################################################
################################################################





#2010
ene
Cabecera<- read_sav("Ingresos laboral/2010/Enero/Enero/Cabecera - Ocupados (1).sav")
resto<- read_sav("Ingresos laboral/2010/Enero/Enero/Resto - Ocupados (1).sav")
area<- read_sav("Ingresos laboral/2010/Enero/Enero/탍ea - Ocupados (1).sav")

feb
Cabecera<- read_sav("Ingresos laboral/2010/Febrero/Febrero/Cabecera - Ocupados (2).sav")
resto<- read_sav("Ingresos laboral/2010/Febrero/Febrero/Resto - Ocupados (2).sav")
area<- read_sav("Ingresos laboral/2010/Febrero/Febrero/탍ea - Ocupados (2).sav")

marz
Cabecera<- read_sav("Ingresos laboral/2010/Marzo/Marzo/Cabecera - Ocupados (3).sav")
resto<- read_sav("Ingresos laboral/2010/Marzo/Marzo/Resto - Ocupados (3).sav")
area<- read_sav("Ingresos laboral/2010/Marzo/Marzo/탍ea - Ocupados (3).sav")

abr
Cabecera<- read_sav("Ingresos laboral/2010/Abril/Abril/Cabecera - Ocupados (4).sav")
resto<- read_sav("Ingresos laboral/2010/Abril/Abril/Resto - Ocupados (4).sav")
area<- read_sav("Ingresos laboral/2010/Abril/Abril/탍ea - Ocupados (4).sav")

may
Cabecera<- read_sav("Ingresos laboral/2010/Mayo/Mayo/Cabecera - Ocupados (5).sav")
resto<- read_sav("Ingresos laboral/2010/Mayo/Mayo/Resto - Ocupados (5).sav")
area<- read_sav("Ingresos laboral/2010/Mayo/Mayo/탍ea - Ocupados (5).sav")

jun
Cabecera<- read_sav("Ingresos laboral/2010/Junio/Junio/Cabecera - Ocupados (6).sav")
resto<- read_sav("Ingresos laboral/2010/Junio/Junio/Resto - Ocupados (6).sav")
area<- read_sav("Ingresos laboral/2010/Junio/Junio/탍ea - Ocupados (6).sav")

juli
Cabecera<- read_sav("Ingresos laboral/2010/Julio/Julio/Cabecera - Ocupados (7).sav")
resto<- read_sav("Ingresos laboral/2010/Julio/Julio/Resto - Ocupados (7).sav")
area<- read_sav("Ingresos laboral/2010/Julio/Julio/탍ea - Ocupados (7).sav")

agos
Cabecera<- read_sav("Ingresos laboral/2010/Agosto/Agosto/Cabecera - Ocupados (8).sav")
resto<- read_sav("Ingresos laboral/2010/Agosto/Agosto/Resto - Ocupados (8).sav")
area<- read_sav("Ingresos laboral/2010/Agosto/Agosto/탍ea - Ocupados (8).sav")

sep
Cabecera<- read_sav("Ingresos laboral/2010/Septiembre/Septiembre/Cabecera - Ocupados (9).sav")
resto<- read_sav("Ingresos laboral/2010/Septiembre/Septiembre/Resto - Ocupados (9).sav")
area<- read_sav("Ingresos laboral/2010/Septiembre/Septiembre/탍ea - Ocupados (9).sav")


oct
Cabecera<- read_sav("Ingresos laboral/2010/Octubre/Octubre/Cabecera - Ocupados (10).sav")
resto<- read_sav("Ingresos laboral/2010/Octubre/Octubre/Resto - Ocupados (10).sav")
area<- read_sav("Ingresos laboral/2010/Octubre/Octubre/탍ea - Ocupados (10).sav")

nov
Cabecera<- read_sav("Ingresos laboral/2010/Noviembre/Noviembre/Cabecera - Ocupados (11).sav")
resto<- read_sav("Ingresos laboral/2010/Noviembre/Noviembre/Resto - Ocupados (11).sav")
area<- read_sav("Ingresos laboral/2010/Noviembre/Noviembre/탍ea - Ocupados (11).sav")

dic
Cabecera<- read_sav("Ingresos laboral/2010/Diciembre/Diciembre/Cabecera - Ocupados (12).sav")
resto<- read_sav("Ingresos laboral/2010/Diciembre/Diciembre/Resto - Ocupados (12).sav")
area<- read_sav("Ingresos laboral/2010/Diciembre/Diciembre/탍ea - Ocupados (12).sav")


################################################################
################################################################
################################################################






#2011
ene  #CONTINUAR SEGUN ESTE DIRECTORIO
Cabecera<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Enero.sav/Enero.sav/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Febrero.sav/Febrero.sav/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Marzo.sav/Marzo.sav/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Abril.sav/Abril.sav/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Mayo.sav/Mayo.sav/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Junio.sav/Junio.sav/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Julio.sav/Julio.sav/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Agosto.sav/Agosto.sav/탍ea - Ocupados.sav")

sep
Cabecera<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Septiembre.sav/Septiembre.sav/탍ea - Ocupados.sav")

oct
Cabecera<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Octubre.sav/Octubre.sav/탍ea - Ocupados.sav")

nov
Cabecera<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Noviembre.sav/Noviembre.sav/탍ea - Ocupados.sav")

dic
Cabecera<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2011/Diciembre.sav/Diciembre.sav/탍ea - Ocupados.sav")




################################################################
################################################################
################################################################



#2012
Enero
Cabecera <- read_sav("Ingresos laboral/2012/Enero/01/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Enero/01/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Enero/01/탍ea - Ocupados.sav")

febrero
Cabecera <- read_sav("Ingresos laboral/2012/Febrero/02/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Febrero/02/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Febrero/02/탍ea - Ocupados.sav")

marzo
Cabecera <- read_sav("Ingresos laboral/2012/Marzo/03/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Marzo/03/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Marzo/03/탍ea - Ocupados.sav")

abril
Cabecera <- read_sav("Ingresos laboral/2012/Abril/04/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Abril/04/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Abril/04/탍ea - Ocupados.sav")

mayo
Cabecera <- read_sav("Ingresos laboral/2012/Mayo/05/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Mayo/05/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Mayo/05/탍ea - Ocupados.sav")

junio
Cabecera <- read_sav("Ingresos laboral/2012/Junio/06/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Junio/06/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Junio/06/탍ea - Ocupados.sav")

julio
Cabecera <- read_sav("Ingresos laboral/2012/Julio/07/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Julio/07/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Julio/07/탍ea - Ocupados.sav")

agosto
Cabecera <- read_sav("Ingresos laboral/2012/Agosto/08/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Agosto/08/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Agosto/08/탍ea - Ocupados.sav")


septiembre
Cabecera <- read_sav("Ingresos laboral/2012/Septiembre/09/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Septiembre/09/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Septiembre/09/탍ea - Ocupados.sav")

octubre
Cabecera <- read_sav("Ingresos laboral/2012/Octubre/10/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Octubre/10/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Octubre/10/탍ea - Ocupados.sav")


noviembre
Cabecera <- read_sav("Ingresos laboral/2012/Noviembre/11/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Noviembre/11/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Noviembre/11/탍ea - Ocupados.sav")

diciembre
Cabecera <- read_sav("Ingresos laboral/2012/Diciembre/12/Cabecera - Ocupados.sav")
resto <- read_sav("Ingresos laboral/2012/Diciembre/12/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2012/Diciembre/12/탍ea - Ocupados.sav")



################################################################
################################################################
################################################################


#2013
ene
Cabecera<- read_sav("Ingresos laboral/2013/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Enero/Enero/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2013/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Febrero/Febrero/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2013/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Marzo/Marzo/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2013/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Abril/Abril/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2013/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Mayo/Mayo/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2013/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Junio/Junio/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2013/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Julio/Julio/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2013/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Agosto/Agosto/탍ea - Ocupados.sav")

sep
Cabecera<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
Cabecera<- read_sav("Ingresos laboral/2013/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Octubre/Octubre/탍ea - Ocupados.sav")

nov
Cabecera<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
Cabecera<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2013/Diciembre/Diciembre/탍ea - Ocupados.sav")






################################################################
################################################################
################################################################


#2014
ene
Cabecera<- read_sav("Ingresos laboral/2014/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Enero/Enero/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2014/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Febrero/Febrero/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2014/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Marzo/Marzo/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2014/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Abril/Abril/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2014/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Mayo/Mayo/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2014/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Junio/Junio/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2014/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Julio/Julio/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2014/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Agosto/Agosto/탍ea - Ocupados.sav")

sep
Cabecera<- read_sav("Ingresos laboral/2014/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
Cabecera<- read_sav("Ingresos laboral/2014/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Octubre/Octubre/탍ea - Ocupados.sav")

nov
Cabecera<- read_sav("Ingresos laboral/2014/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
Cabecera<- read_sav("Ingresos laboral/2014/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2014/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2014/Diciembre/Diciembre/탍ea - Ocupados.sav")



################################################################
################################################################
################################################################


#2015
ene
Cabecera<- read_sav("Ingresos laboral/2015/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Enero/Enero/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2015/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Febrero/Febrero/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2015/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Marzo/Marzo/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2015/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Abril/Abril/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2015/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Mayo/Mayo/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2015/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Junio/Junio/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2015/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Julio/Julio/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2015/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Agosto/Agosto/탍ea - Ocupados.sav")

sep
Cabecera<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
Cabecera<- read_sav("Ingresos laboral/2015/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Octubre/Octubre/탍ea - Ocupados.sav")

nov
Cabecera<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
Cabecera<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2015/Diciembre/Diciembre/탍ea - Ocupados.sav")









################################################################
################################################################
################################################################


#2016
ene
Cabecera<- read_sav("Ingresos laboral/2016/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Enero/Enero/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2016/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Febrero/Febrero/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2016/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Marzo/Marzo/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2016/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Abril/Abril/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2016/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Mayo/Mayo/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2016/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Junio/Junio/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2016/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Julio/Julio/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2016/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Agosto/Agosto/탍ea - Ocupados.sav")

sep
Cabecera<- read_sav("Ingresos laboral/2016/Septiembre/Septiembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Septiembre/Septiembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Septiembre/Septiembre/탍ea - Ocupados.sav")


oct
Cabecera<- read_sav("Ingresos laboral/2016/Octubre/Octubre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Octubre/Octubre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Octubre/Octubre/탍ea - Ocupados.sav")

nov
Cabecera<- read_sav("Ingresos laboral/2016/Noviembre/Noviembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Noviembre/Noviembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Noviembre/Noviembre/탍ea - Ocupados.sav")

dic
Cabecera<- read_sav("Ingresos laboral/2016/Diciembre/Diciembre/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2016/Diciembre/Diciembre/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2016/Diciembre/Diciembre/탍ea - Ocupados.sav")




################################################################
################################################################
################################################################


#2017
ene
Cabecera<- read_sav("Ingresos laboral/2017/Enero/Enero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Enero/Enero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Enero/Enero/탍ea - Ocupados.sav")

feb
Cabecera<- read_sav("Ingresos laboral/2017/Febrero/Febrero/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Febrero/Febrero/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Febrero/Febrero/탍ea - Ocupados.sav")

marz
Cabecera<- read_sav("Ingresos laboral/2017/Marzo/Marzo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Marzo/Marzo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Marzo/Marzo/탍ea - Ocupados.sav")

abr
Cabecera<- read_sav("Ingresos laboral/2017/Abril/Abril/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Abril/Abril/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Abril/Abril/탍ea - Ocupados.sav")

may
Cabecera<- read_sav("Ingresos laboral/2017/Mayo/Mayo/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Mayo/Mayo/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Mayo/Mayo/탍ea - Ocupados.sav")

jun
Cabecera<- read_sav("Ingresos laboral/2017/Junio/Junio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Junio/Junio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Junio/Junio/탍ea - Ocupados.sav")

juli
Cabecera<- read_sav("Ingresos laboral/2017/Julio/Julio/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Julio/Julio/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Julio/Julio/탍ea - Ocupados.sav")

agos
Cabecera<- read_sav("Ingresos laboral/2017/Agosto/Agosto/Cabecera - Ocupados.sav")
resto<- read_sav("Ingresos laboral/2017/Agosto/Agosto/Resto - Ocupados.sav")
area<- read_sav("Ingresos laboral/2017/Agosto/Agosto/햞ea - Ocupados.sav")

sep #
Cabecera <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Septiembre.dta/Septiembre.dta/area.csv")


oct
Cabecera <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Octubre.dta/Octubre.dta/area.csv")

nov
Cabecera <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Noviembre.dta/Noviembre.dta/area.csv")

dic
Cabecera <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2017/Diciembre.dta/Diciembre.dta/area.csv")





################################################################
################################################################
################################################################


#2018

#Mirar cuales funcionas con formato dta
ene
Cabecera <- read.csv("Ingresos laboral/2018/Enero.dta/Enero.dta/cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Enero.dta/Enero.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Enero.dta/Enero.dta/area.csv")

feb
Cabecera <- read.csv("Ingresos laboral/2018/Febrero.dta/Febrero.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Febrero.dta/Febrero.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Febrero.dta/Febrero.dta/area.csv")

mar
Cabecera <- read.csv("Ingresos laboral/2018/Marzo.dta/Marzo.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Marzo.dta/Marzo.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Marzo.dta/Marzo.dta/area.csv")

abr
Cabecera <- read.csv("Ingresos laboral/2018/Abril.dta/Abril.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Abril.dta/Abril.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Abril.dta/Abril.dta/area.csv")

may
Cabecera <- read.csv("Ingresos laboral/2018/Mayo.dta/Mayo.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Mayo.dta/Mayo.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Mayo.dta/Mayo.dta/area.csv")

jun
Cabecera <- read.csv("Ingresos laboral/2018/Junio.dta/Junio.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Junio.dta/Junio.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Junio.dta/Junio.dta/area.csv")

jul
Cabecera <-read_dta("Ingresos laboral/2018/Julio.dta/Julio.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2018/Julio.dta/Julio.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2018/Julio.dta/Julio.dta/탍ea - Ocupados.dta")

ago
Cabecera <-read_dta("Ingresos laboral/2018/Agosto.dta/Agosto.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2018/Agosto.dta/Agosto.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2018/Agosto.dta/Agosto.dta/탍ea - Ocupados.dta")

sep
Cabecera <-read_dta("Ingresos laboral/2018/Septiembre.dta/Septiembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2018/Septiembre.dta/Septiembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2018/Septiembre.dta/Septiembre.dta/탍ea - Ocupados.dta")

oct
Cabecera <- read.csv("Ingresos laboral/2018/Octubre.dta/Octubre.dta/Cabecera.csv")
resto <- read.csv("Ingresos laboral/2018/Octubre.dta/Octubre.dta/resto.csv")
area <- read.csv("Ingresos laboral/2018/Octubre.dta/Octubre.dta/area.csv")

nov
Cabecera <-read_dta("Ingresos laboral/2018/Noviembre.dta/Noviembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2018/Noviembre.dta/Noviembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2018/Noviembre.dta/Noviembre.dta/탍ea - Ocupados.dta")

dic
Cabecera <-read_dta("Ingresos laboral/2018/Diciembre.dta/Diciembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2018/Diciembre.dta/Diciembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2018/Diciembre.dta/Diciembre.dta/탍ea - Ocupados.dta")






################################################################
################################################################
################################################################


#2019

ene
Cabecera <-read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Enero.dta/Enero.dta/탍ea - Ocupados.dta")

feb
Cabecera <-read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Febrero.dta/Febrero.dta/area.csv")

mar
Cabecera <-read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Marzo.dta/Marzo.dta/탍ea - Ocupados.dta")

abr
Cabecera <-read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Abril.dta/Abril.dta/탍ea - Ocupados.dta")

may
Cabecera <-read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Mayo.dta/Mayo.dta/탍ea - Ocupados.dta")

jun
Cabecera <-read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Junio.dta/Junio.dta/area.csv")

jul
Cabecera <-read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Julio.dta/Julio.dta/탍ea - Ocupados.dta")

ago
Cabecera <-read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Agosto.dta/Agosto.dta/탍ea - Ocupados.dta")

sep
Cabecera <-read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Septiembre.dta/Septiembre.dta/탍ea - Ocupados.dta")

oct
Cabecera <-read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Octubre.dta/Octubre.dta/area.csv")

nov
Cabecera <-read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/Cabecera.csv")
resto<- read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/resto.csv")
area<- read.csv("Ingresos laboral/2019/Noviembre.dta/Noviembre.dta/area.csv")

dic
Cabecera <-read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/Cabecera - Ocupados.dta")
resto<- read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/Resto - Ocupados.dta")
area<- read_dta("Ingresos laboral/2019/Diciembre.dta/Diciembre.dta/탍ea - Ocupados.dta")



