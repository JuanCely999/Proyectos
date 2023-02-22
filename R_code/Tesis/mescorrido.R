##############################################################################
###Desagregacion bases de datos ingresos laborales desde GEIH##
###########################Trimestres y meses ###############################
#http://microdatos.dane.gov.co/index.php/catalog/207/get_microdata
######################Autor: Juan Pablo Cely#################################
###############################18-03-2020####################################

rm(list=ls())
library(haven)
library(dplyr)
library(tidyverse) 
library(sqldf)
library(data.table)
library(tidyr)

setwd("~/Documents/Tesis/Bases de datos/meses/")
#dir.resul <- "~/Documents/Tesis/Bases de datos/mestrim corrido/"
#setwd(dir.resul)


##########Subir bases de datos##############

#2009
ene2009<- read.csv("enero2009.csv")
ene2009<- data.frame(ene2009[,c(2:3)])
names(ene2009)<- c("DPTO","suma200901")
#feb
feb2009<- read.csv("Febrero2009.csv")
feb2009<- data.frame(feb2009[,c(3)])
names(feb2009)<- c("suma200902")
write.csv(feb2009, file = "feb2009.csv")

#mar
mar2009<- read.csv("Marzo2009.csv")
mar2009<- data.frame(mar2009[,c(3)])
names(mar2009)<- c("suma200903")

#abr
abr2009<- read.csv("Abril2009.csv")
abr2009<- data.frame(abr2009[,c(3)])
names(abr2009)<- c("suma200904")

#may
may2009<- read.csv("Mayo2009.csv")
may2009<- data.frame(may2009[,c(3)])
names(may2009)<- c("suma200905")

#jun
jun2009<- read.csv("Junio2009.csv")
jun2009<- data.frame(jun2009[,c(3)])
names(jun2009)<- c("suma200906")

#jul
jul2009<- read.csv("Julio2009.csv")
jul2009<- data.frame(jul2009[,c(3)])
names(jul2009)<- c("suma200907")

#ago
ago2009<- read.csv("Agosto2009.csv")
ago2009<- data.frame(ago2009[,c(3)])
names(ago2009)<- c("suma200908")

#sep
sep2009<- read.csv("Septiembre2009.csv")
sep2009<- data.frame(sep2009[,c(3)])
names(sep2009)<- c("suma200909")

#oct
oct2009<- read.csv("Octubre2009.csv")
oct2009<- data.frame(oct2009[,c(3)])
names(oct2009)<- c("suma200910")

#nov
nov2009<- read.csv("Noviembre2009.csv")
nov2009<- data.frame(nov2009[,c(3)])
names(nov2009)<- c("suma200911")

#dic
dic2009<- read.csv("Diciembre2009.csv")
dic2009<- data.frame(dic2009[,c(3)])
names(dic2009)<- c("suma200912")


#------------------------------------------- #
#2010
ene2010<- read.csv("enero2010.csv")
ene2010<- data.frame(ene2010[,c(3)])
names(ene2010)<- c("suma201001")
#feb
feb2010<- read.csv("Febrero2010.csv")
feb2010<- data.frame(feb2010[,c(3)])
names(feb2010)<- c("suma201002")

#mar
mar2010<- read.csv("Marzo2010.csv")
mar2010<- data.frame(mar2010[,c(3)])
names(mar2010)<- c("suma201003")

#abr
abr2010<- read.csv("Abril2010.csv")
abr2010<- data.frame(abr2010[,c(3)])
names(abr2010)<- c("suma201004")

#may
may2010<- read.csv("Mayo2010.csv")
may2010<- data.frame(may2010[,c(3)])
names(may2010)<- c("suma201005")

#jun
jun2010<- read.csv("Junio2010.csv")
jun2010<- data.frame(jun2010[,c(3)])
names(jun2010)<- c("suma201006")

#jul
jul2010<- read.csv("Julio2010.csv")
jul2010<- data.frame(jul2010[,c(3)])
names(jul2010)<- c("suma201007")

#ago
ago2010<- read.csv("Agosto2010.csv")
ago2010<- data.frame(ago2010[,c(3)])
names(ago2010)<- c("suma201008")

#sep
sep2010<- read.csv("Septiembre2010.csv")
sep2010<- data.frame(sep2010[,c(3)])
names(sep2010)<- c("suma201009")

#oct
oct2010<- read.csv("Octubre2010.csv")
oct2010<- data.frame(oct2010[,c(3)])
names(oct2010)<- c("suma201010")

#nov
nov2010<- read.csv("Noviembre2010.csv")
nov2010<- data.frame(nov2010[,c(3)])
names(nov2010)<- c("suma201011")

#dic
dic2010<- read.csv("Diciembre2010.csv")
dic2010<- data.frame(dic2010[,c(3)])
names(dic2010)<- c("suma201012")


#------------------------------------------- #
#2011
ene2011<- read.csv("enero2011.csv")
ene2011<- data.frame(ene2011[,c(3)])
names(ene2011)<- c("suma201101")
#feb
feb2011<- read.csv("Febrero2011.csv")
feb2011<- data.frame(feb2011[,c(3)])
names(feb2011)<- c("suma201102")

#mar
mar2011<- read.csv("Marzo2011.csv")
mar2011<- data.frame(mar2011[,c(3)])
names(mar2011)<- c("suma201103")

#abr
abr2011<- read.csv("Abril2011.csv")
abr2011<- data.frame(abr2011[,c(3)])
names(abr2011)<- c("suma201104")

#may
may2011<- read.csv("Mayo2011.csv")
may2011<- data.frame(may2011[,c(3)])
names(may2011)<- c("suma201105")

#jun
jun2011<- read.csv("Junio2011.csv")
jun2011<- data.frame(jun2011[,c(3)])
names(jun2011)<- c("suma201106")

#jul
jul2011<- read.csv("Julio2011.csv")
jul2011<- data.frame(jul2011[,c(3)])
names(jul2011)<- c("suma201107")

#ago
ago2011<- read.csv("Agosto2011.csv")
ago2011<- data.frame(ago2011[,c(3)])
names(ago2011)<- c("suma201108")

#sep
sep2011<- read.csv("Septiembre2011.csv")
sep2011<- data.frame(sep2011[,c(3)])
names(sep2011)<- c("suma201109")

#oct
oct2011<- read.csv("Octubre2011.csv")
oct2011<- data.frame(oct2011[,c(3)])
names(oct2011)<- c("suma201110")

#nov
nov2011<- read.csv("Noviembre2011.csv")
nov2011<- data.frame(nov2011[,c(3)])
names(nov2011)<- c("suma201111")

#dic
dic2011<- read.csv("Diciembre2011.csv")
dic2011<- data.frame(dic2011[,c(3)])
names(dic2011)<- c("suma201112")




#------------------------------------------- #
#2012
ene2012<- read.csv("enero2012.csv")
ene2012<- data.frame(ene2012[,c(3)])
names(ene2012)<- c("suma201201")
#feb
feb2012<- read.csv("Febrero2012.csv")
feb2012<- data.frame(feb2012[,c(3)])
names(feb2012)<- c("suma201202")

#mar
mar2012<- read.csv("Marzo2012.csv")
mar2012<- data.frame(mar2012[,c(3)])
names(mar2012)<- c("suma201203")

#abr
abr2012<- read.csv("Abril2012.csv")
abr2012<- data.frame(abr2012[,c(3)])
names(abr2012)<- c("suma201204")

#may
may2012<- read.csv("Mayo2012.csv")
may2012<- data.frame(may2012[,c(3)])
names(may2012)<- c("suma201205")

#jun
jun2012<- read.csv("Junio2012.csv")
jun2012<- data.frame(jun2012[,c(3)])
names(jun2012)<- c("suma201206")

#jul
jul2012<- read.csv("Julio2012.csv")
jul2012<- data.frame(jul2012[,c(3)])
names(jul2012)<- c("suma201207")

#ago
ago2012<- read.csv("Agosto2012.csv")
ago2012<- data.frame(ago2012[,c(3)])
names(ago2012)<- c("suma201208")

#sep
sep2012<- read.csv("Septiembre2012.csv")
sep2012<- data.frame(sep2012[,c(3)])
names(sep2012)<- c("suma201209")

#oct
oct2012<- read.csv("Octubre2012.csv")
oct2012<- data.frame(oct2012[,c(3)])
names(oct2012)<- c("suma201210")

#nov
nov2012<- read.csv("Noviembre2012.csv")
nov2012<- data.frame(nov2012[,c(3)])
names(nov2012)<- c("suma201211")

#dic
dic2012<- read.csv("Diciembre2012.csv")
dic2012<- data.frame(dic2012[,c(3)])
names(dic2012)<- c("suma201212")




#------------------------------------------- #
#2013
ene2013<- read.csv("enero2013.csv")
ene2013<- data.frame(ene2013[,c(3)])
names(ene2013)<- c("suma201301")
#feb
feb2013<- read.csv("Febrero2013.csv")
feb2013<- data.frame(feb2013[,c(3)])
names(feb2013)<- c("suma201302")

#mar
mar2013<- read.csv("Marzo2013.csv")
mar2013<- data.frame(mar2013[,c(3)])
names(mar2013)<- c("suma201303")

#abr
abr2013<- read.csv("Abril2013.csv")
abr2013<- data.frame(abr2013[,c(3)])
names(abr2013)<- c("suma201304")

#may
may2013<- read.csv("Mayo2013.csv")
may2013<- data.frame(may2013[,c(3)])
names(may2013)<- c("suma201305")

#jun
jun2013<- read.csv("Junio2013.csv")
jun2013<- data.frame(jun2013[,c(3)])
names(jun2013)<- c("suma201306")

#jul
jul2013<- read.csv("Julio2013.csv")
jul2013<- data.frame(jul2013[,c(3)])
names(jul2013)<- c("suma201307")

#ago
ago2013<- read.csv("Agosto2013.csv")
ago2013<- data.frame(ago2013[,c(3)])
names(ago2013)<- c("suma201308")

#sep
sep2013<- read.csv("Septiembre2013.csv")
sep2013<- data.frame(sep2013[,c(3)])
names(sep2013)<- c("suma201309")

#oct
oct2013<- read.csv("Octubre2013.csv")
oct2013<- data.frame(oct2013[,c(3)])
names(oct2013)<- c("suma201310")

#nov
nov2013<- read.csv("Noviembre2013.csv")
nov2013<- data.frame(nov2013[,c(3)])
names(nov2013)<- c("suma201311")

#dic
dic2013<- read.csv("Diciembre2013.csv")
dic2013<- data.frame(dic2013[,c(3)])
names(dic2013)<- c("suma201312")




#------------------------------------------- #
#2014
ene2014<- read.csv("enero2014.csv")
ene2014<- data.frame(ene2014[,c(3)])
names(ene2014)<- c("suma201401")
#feb
feb2014<- read.csv("Febrero2014.csv")
feb2014<- data.frame(feb2014[,c(3)])
names(feb2014)<- c("suma201402")

#mar
mar2014<- read.csv("Marzo2014.csv")
mar2014<- data.frame(mar2014[,c(3)])
names(mar2014)<- c("suma201403")

#abr
abr2014<- read.csv("Abril2014.csv")
abr2014<- data.frame(abr2014[,c(3)])
names(abr2014)<- c("suma201404")

#may
may2014<- read.csv("Mayo2014.csv")
may2014<- data.frame(may2014[,c(3)])
names(may2014)<- c("suma201405")

#jun
jun2014<- read.csv("Junio2014.csv")
jun2014<- data.frame(jun2014[,c(3)])
names(jun2014)<- c("suma201406")

#jul
jul2014<- read.csv("Julio2014.csv")
jul2014<- data.frame(jul2014[,c(3)])
names(jul2014)<- c("suma201407")

#ago
ago2014<- read.csv("Agosto2014.csv")
ago2014<- data.frame(ago2014[,c(3)])
names(ago2014)<- c("suma201408")

#sep
sep2014<- read.csv("Septiembre2014.csv")
sep2014<- data.frame(sep2014[,c(3)])
names(sep2014)<- c("suma201409")

#oct
oct2014<- read.csv("Octubre2014.csv")
oct2014<- data.frame(oct2014[,c(3)])
names(oct2014)<- c("suma201410")

#nov
nov2014<- read.csv("Noviembre2014.csv")
nov2014<- data.frame(nov2014[,c(3)])
names(nov2014)<- c("suma201411")

#dic
dic2014<- read.csv("Diciembre2014.csv")
dic2014<- data.frame(dic2014[,c(3)])
names(dic2014)<- c("suma201412")




#------------------------------------------- #
#2015
ene2015<- read.csv("enero2015.csv")
ene2015<- data.frame(ene2015[,c(3)])
names(ene2015)<- c("suma201501")
#feb
feb2015<- read.csv("Febrero2015.csv")
feb2015<- data.frame(feb2015[,c(3)])
names(feb2015)<- c("suma201502")

#mar
mar2015<- read.csv("Marzo2015.csv")
mar2015<- data.frame(mar2015[,c(3)])
names(mar2015)<- c("suma201503")

#abr
abr2015<- read.csv("Abril2015.csv")
abr2015<- data.frame(abr2015[,c(3)])
names(abr2015)<- c("suma201504")

#may
may2015<- read.csv("Mayo2015.csv")
may2015<- data.frame(may2015[,c(3)])
names(may2015)<- c("suma201505")

#jun
jun2015<- read.csv("Junio2015.csv")
jun2015<- data.frame(jun2015[,c(3)])
names(jun2015)<- c("suma201506")

#jul
jul2015<- read.csv("Julio2015.csv")
jul2015<- data.frame(jul2015[,c(3)])
names(jul2015)<- c("suma201507")

#ago
ago2015<- read.csv("Agosto2015.csv")
ago2015<- data.frame(ago2015[,c(3)])
names(ago2015)<- c("suma201508")

#sep
sep2015<- read.csv("Septiembre2015.csv")
sep2015<- data.frame(sep2015[,c(3)])
names(sep2015)<- c("suma201509")

#oct
oct2015<- read.csv("Octubre2015.csv")
oct2015<- data.frame(oct2015[,c(3)])
names(oct2015)<- c("suma201510")

#nov
nov2015<- read.csv("Noviembre2015.csv")
nov2015<- data.frame(nov2015[,c(3)])
names(nov2015)<- c("suma201511")

#dic
dic2015<- read.csv("Diciembre2015.csv")
dic2015<- data.frame(dic2015[,c(3)])
names(dic2015)<- c("suma201512")




#------------------------------------------- #
#2016
ene2016<- read.csv("enero2016.csv")
ene2016<- data.frame(ene2016[,c(3)])
names(ene2016)<- c("suma201601")
#feb
feb2016<- read.csv("Febrero2016.csv")
feb2016<- data.frame(feb2016[,c(3)])
names(feb2016)<- c("suma201602")

#mar
mar2016<- read.csv("Marzo2016.csv")
mar2016<- data.frame(mar2016[,c(3)])
names(mar2016)<- c("suma201603")

#abr
abr2016<- read.csv("Abril2016.csv")
abr2016<- data.frame(abr2016[,c(3)])
names(abr2016)<- c("suma201604")

#may
may2016<- read.csv("Mayo2016.csv")
may2016<- data.frame(may2016[,c(3)])
names(may2016)<- c("suma201605")

#jun
jun2016<- read.csv("Junio2016.csv")
jun2016<- data.frame(jun2016[,c(3)])
names(jun2016)<- c("suma201606")

#jul
jul2016<- read.csv("Julio2016.csv")
jul2016<- data.frame(jul2016[,c(3)])
names(jul2016)<- c("suma201607")

#ago
ago2016<- read.csv("Agosto2016.csv")
ago2016<- data.frame(ago2016[,c(3)])
names(ago2016)<- c("suma201608")

#sep
sep2016<- read.csv("Septiembre2016.csv")
sep2016<- data.frame(sep2016[,c(3)])
names(sep2016)<- c("suma201609")

#oct
oct2016<- read.csv("Octubre2016.csv")
oct2016<- data.frame(oct2016[,c(3)])
names(oct2016)<- c("suma201610")

#nov
nov2016<- read.csv("Noviembre2016.csv")
nov2016<- data.frame(nov2016[,c(3)])
names(nov2016)<- c("suma201611")

#dic
dic2016<- read.csv("Diciembre2016.csv")
dic2016<- data.frame(dic2016[,c(3)])
names(dic2016)<- c("suma201612")




#------------------------------------------- #
#2017
ene2017<- read.csv("enero2017.csv")
ene2017<- data.frame(ene2017[,c(3)])
names(ene2017)<- c("suma201701")
#feb
feb2017<- read.csv("Febrero2017.csv")
feb2017<- data.frame(feb2017[,c(3)])
names(feb2017)<- c("suma201702")

#mar
mar2017<- read.csv("Marzo2017.csv")
mar2017<- data.frame(mar2017[,c(3)])
names(mar2017)<- c("suma201703")

#abr
abr2017<- read.csv("Abril2017.csv")
abr2017<- data.frame(abr2017[,c(3)])
names(abr2017)<- c("suma201704")

#may
may2017<- read.csv("Mayo2017.csv")
may2017<- data.frame(may2017[,c(3)])
names(may2017)<- c("suma201705")

#jun
jun2017<- read.csv("Junio2017.csv")
jun2017<- data.frame(jun2017[,c(3)])
names(jun2017)<- c("suma201706")

#jul
jul2017<- read.csv("Julio2017.csv")
jul2017<- data.frame(jul2017[,c(3)])
names(jul2017)<- c("suma201707")

#ago
ago2017<- read.csv("Agosto2017.csv")
ago2017<- data.frame(ago2017[,c(3)])
names(ago2017)<- c("suma201708")

#sep
sep2017<- read.csv("Septiembre2017.csv")
sep2017<- data.frame(sep2017[,c(3)])
names(sep2017)<- c("suma201709")

#oct
oct2017<- read.csv("Octubre2017.csv")
oct2017<- data.frame(oct2017[,c(3)])
names(oct2017)<- c("suma201710")

#nov
nov2017<- read.csv("Noviembre2017.csv")
nov2017<- data.frame(nov2017[,c(3)])
names(nov2017)<- c("suma201711")

#dic
dic2017<- read.csv("Diciembre2017.csv")
dic2017<- data.frame(dic2017[,c(3)])
names(dic2017)<- c("suma201712")




#------------------------------------------- #
#2018
ene2018<- read.csv("enero2018.csv")
ene2018<- data.frame(ene2018[,c(3)])
names(ene2018)<- c("suma201801")
#feb
feb2018<- read.csv("Febrero2018.csv")
feb2018<- data.frame(feb2018[,c(3)])
names(feb2018)<- c("suma201802")

#mar
mar2018<- read.csv("Marzo2018.csv")
mar2018<- data.frame(mar2018[,c(3)])
names(mar2018)<- c("suma201803")

#abr
abr2018<- read.csv("Abril2018.csv")
abr2018<- data.frame(abr2018[,c(3)])
names(abr2018)<- c("suma201804")

#may
may2018<- read.csv("Mayo2018.csv")
may2018<- data.frame(may2018[,c(3)])
names(may2018)<- c("suma201805")

#jun
jun2018<- read.csv("Junio2018.csv")
jun2018<- data.frame(jun2018[,c(3)])
names(jun2018)<- c("suma201806")

#jul
jul2018<- read.csv("Julio2018.csv")
jul2018<- data.frame(jul2018[,c(3)])
names(jul2018)<- c("suma201807")

#ago
ago2018<- read.csv("Agosto2018.csv")
ago2018<- data.frame(ago2018[,c(3)])
names(ago2018)<- c("suma201808")

#sep
sep2018<- read.csv("Septiembre2018.csv")
sep2018<- data.frame(sep2018[,c(3)])
names(sep2018)<- c("suma201809")

#oct
oct2018<- read.csv("Octubre2018.csv")
oct2018<- data.frame(oct2018[,c(3)])
names(oct2018)<- c("suma201810")

#nov
nov2018<- read.csv("Noviembre2018.csv")
nov2018<- data.frame(nov2018[,c(3)])
names(nov2018)<- c("suma201811")

#dic
dic2018<- read.csv("Diciembre2018.csv")
dic2018<- data.frame(dic2018[,c(3)])
names(dic2018)<- c("suma201812")


#------------------------------------------- #
#2019
ene2019<- read.csv("enero2019.csv")
ene2019<- data.frame(ene2019[,c(3)])
names(ene2019)<- c("suma201901")
#feb
feb2019<- read.csv("Febrero2019.csv")
feb2019<- data.frame(feb2019[,c(3)])
names(feb2019)<- c("suma201902")

#mar
mar2019<- read.csv("Marzo2019.csv")
mar2019<- data.frame(mar2019[,c(3)])
names(mar2019)<- c("suma201903")

#abr
abr2019<- read.csv("Abril2019.csv")
abr2019<- data.frame(abr2019[,c(3)])
names(abr2019)<- c("suma201904")

#may
may2019<- read.csv("Mayo2019.csv")
may2019<- data.frame(may2019[,c(3)])
names(may2019)<- c("suma201905")

#jun
jun2019<- read.csv("Junio2019.csv")
jun2019<- data.frame(jun2019[,c(3)])
names(jun2019)<- c("suma201906")

#jul
jul2019<- read.csv("Julio2019.csv")
jul2019<- data.frame(jul2019[,c(3)])
names(jul2019)<- c("suma201907")

#ago
ago2019<- read.csv("Agosto2019.csv")
ago2019<- data.frame(ago2019[,c(3)])
names(ago2019)<- c("suma201908")

#sep
sep2019<- read.csv("Septiembre2019.csv")
sep2019<- data.frame(sep2019[,c(3)])
names(sep2019)<- c("suma201909")

#oct
oct2019<- read.csv("Octubre2019.csv")
oct2019<- data.frame(oct2019[,c(3)])
names(oct2019)<- c("suma201910")

#nov
nov2019<- read.csv("Noviembre2019.csv")
nov2019<- data.frame(nov2019[,c(3)])
names(nov2019)<- c("suma201911")

#dic
dic2019<- read.csv("Diciembre2019.csv")
dic2019<- data.frame(dic2019[,c(3)])
names(dic2019)<- c("suma201912")


mescorrido<- cbind(ene2009,feb2009,mar2009,abr2009,may2009,jun2009,jul2009,ago2009,sep2009,oct2009,nov2009,dic2009,
                   ene2010,feb2010,mar2010,abr2010,may2010,jun2010,jul2010,ago2010,sep2010,oct2010,nov2010,dic2010,
                   ene2011,feb2011,mar2011,abr2011,may2011,jun2011,jul2011,ago2011,sep2011,oct2011,nov2011,dic2011,
                   ene2012,feb2012,mar2012,abr2012,may2012,jun2012,jul2012,ago2012,sep2012,oct2012,nov2012,dic2012,
                   ene2013,feb2013,mar2013,abr2013,may2013,jun2013,jul2013,ago2013,sep2013,oct2013,nov2013,dic2013,
                   ene2014,feb2014,mar2014,abr2014,may2014,jun2014,jul2014,ago2014,sep2014,oct2014,nov2014,dic2014,
                   ene2015,feb2015,mar2015,abr2015,may2015,jun2015,jul2015,ago2015,sep2015,oct2015,nov2015,dic2015,
                   ene2016,feb2016,mar2016,abr2016,may2016,jun2016,jul2016,ago2016,sep2016,oct2016,nov2016,dic2016,
                   ene2017,feb2017,mar2017,abr2017,may2017,jun2017,jul2017,ago2017,sep2017,oct2017,nov2017,dic2017,
                   ene2018,feb2018,mar2018,abr2018,may2018,jun2018,jul2018,ago2018,sep2018,oct2018,nov2018,dic2018,
                   ene2019,feb2019,mar2019,abr2019,may2019,jun2019,jul2019,ago2019,sep2019,oct2019,nov2019,dic2019)


#write.csv(mescorrido, file = "mesesnormales.csv")


####################################################################################################################
####################################################################################################################
####################################################################################################################
###############################################MESES CORRIDOS#######################################################
####################################################################################################################
mes <-mescorrido %>%                                #         2010          #
                     mutate(l2010 =   suma200902+suma200903+suma200904+suma200905+suma200906+suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001) %>% 
                     mutate(ll2010 =  suma200903+suma200904+suma200905+suma200906+suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002) %>% 
                     mutate(lll2010 = suma200904+suma200905+suma200906+suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003) %>% 
                     mutate(lv2010 =  suma200905+suma200906+suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004) %>% 
                     mutate(v2010 =   suma200906+suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005) %>% 
                     mutate(vl2010 =  suma200907+suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006) %>% 
                     mutate(vll2010 = suma200908+suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007) %>% 
                     mutate(vlll2010 =suma200909+suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008) %>% 
                     mutate(lx2010 =  suma200910+suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009) %>% 
                     mutate(x2010 =   suma200911+suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010) %>% 
                     mutate(xl2010 =  suma200912+suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011) %>% 
                     mutate(xll2010 = suma201001+suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012) %>% 
                                                    #       2011            #
  mutate(l2011 =   suma201002+suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101) %>% 
  mutate(ll2011 =  suma201003+suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102) %>% 
  mutate(lll2011 = suma201004+suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103) %>% 
  mutate(lv2011 =  suma201005+suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104) %>% 
  mutate(v2011 =   suma201006+suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105) %>% 
  mutate(vl2011 =  suma201007+suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106) %>% 
  mutate(vll2011 = suma201008+suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107) %>% 
  mutate(vlll2011 =suma201009+suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108) %>% 
  mutate(lx2011 =  suma201010+suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109) %>% 
  mutate(x2011 =   suma201011+suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110) %>% 
  mutate(xl2011 =  suma201012+suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111) %>% 
  mutate(xll2011 = suma201101+suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112) %>% 
  #       2012            #
  mutate(l2012 =   suma201102+suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201) %>% 
    mutate(ll2012 =  suma201103+suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202) %>% 
    mutate(lll2012 = suma201104+suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203) %>% 
    mutate(lv2012 =  suma201105+suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204) %>% 
    mutate(v2012 =   suma201106+suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205) %>% 
    mutate(vl2012 =  suma201107+suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206) %>% 
    mutate(vll2012 = suma201108+suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207) %>% 
    mutate(vlll2012 =suma201109+suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208) %>% 
    mutate(lx2012 =  suma201110+suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209) %>% 
    mutate(x2012 =   suma201111+suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210) %>% 
    mutate(xl2012 =  suma201112+suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211) %>% 
    mutate(xll2012 = suma201201+suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212) %>% 
  #       2013            #
  mutate(l2013 =   suma201202+suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301) %>% 
    mutate(ll2013 =  suma201203+suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302) %>% 
    mutate(lll2013 = suma201204+suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303) %>% 
    mutate(lv2013 =  suma201205+suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304) %>% 
    mutate(v2013 =   suma201206+suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305) %>% 
    mutate(vl2013 =  suma201207+suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306) %>% 
    mutate(vll2013 = suma201208+suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307) %>% 
    mutate(vlll2013 =suma201209+suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308) %>% 
    mutate(lx2013 =  suma201210+suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309) %>% 
    mutate(x2013 =   suma201211+suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310) %>% 
    mutate(xl2013 =  suma201212+suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311) %>% 
    mutate(xll2013 = suma201301+suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312) %>% 
  #       2014            #
  mutate(l2014 =   suma201302+suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401) %>% 
    mutate(ll2014 =  suma201303+suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402) %>% 
    mutate(lll2014 = suma201304+suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403) %>% 
    mutate(lv2014 =  suma201305+suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404) %>% 
    mutate(v2014 =   suma201306+suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405) %>% 
    mutate(vl2014 =  suma201307+suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406) %>% 
    mutate(vll2014 = suma201308+suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407) %>% 
    mutate(vlll2014 =suma201309+suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408) %>% 
    mutate(lx2014 =  suma201310+suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409) %>% 
    mutate(x2014 =   suma201311+suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410) %>% 
    mutate(xl2014 =  suma201312+suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411) %>% 
    mutate(xll2014 = suma201401+suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412) %>% 
  #       2015            #
  mutate(l2015 =   suma201402+suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501) %>% 
    mutate(ll2015 =  suma201403+suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502) %>% 
    mutate(lll2015 = suma201404+suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503) %>% 
    mutate(lv2015 =  suma201405+suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504) %>% 
    mutate(v2015 =   suma201406+suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505) %>% 
    mutate(vl2015 =  suma201407+suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506) %>% 
    mutate(vll2015 = suma201408+suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507) %>% 
    mutate(vlll2015 =suma201409+suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508) %>% 
    mutate(lx2015 =  suma201410+suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509) %>% 
    mutate(x2015 =   suma201411+suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510) %>% 
    mutate(xl2015 =  suma201412+suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511) %>% 
    mutate(xll2015 = suma201501+suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512) %>% 
  #       2016            #
  mutate(l2016 =   suma201502+suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601) %>% 
    mutate(ll2016 =  suma201503+suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602) %>% 
    mutate(lll2016 = suma201504+suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603) %>% 
    mutate(lv2016 =  suma201505+suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604) %>% 
    mutate(v2016 =   suma201506+suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605) %>% 
    mutate(vl2016 =  suma201507+suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606) %>% 
    mutate(vll2016 = suma201508+suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607) %>% 
    mutate(vlll2016 =suma201509+suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608) %>% 
    mutate(lx2016 =  suma201510+suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609) %>% 
    mutate(x2016 =   suma201511+suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610) %>% 
    mutate(xl2016 =  suma201512+suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611) %>% 
    mutate(xll2016 = suma201601+suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612) %>% 
  #       2017            #
  mutate(l2017 =   suma201602+suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701) %>% 
    mutate(ll2017 =  suma201603+suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702) %>% 
    mutate(lll2017 = suma201604+suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703) %>% 
    mutate(lv2017 =  suma201605+suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704) %>% 
    mutate(v2017 =   suma201606+suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705) %>% 
    mutate(vl2017 =  suma201607+suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706) %>% 
    mutate(vll2017 = suma201608+suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707) %>% 
    mutate(vlll2017 =suma201609+suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708) %>% 
    mutate(lx2017 =  suma201610+suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709) %>% 
    mutate(x2017 =   suma201611+suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710) %>% 
    mutate(xl2017 =  suma201612+suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711) %>% 
    mutate(xll2017 = suma201701+suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712) %>% 
  #       2018            #
  mutate(l2018 =   suma201702+suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801) %>% 
    mutate(ll2018 =  suma201703+suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802) %>% 
    mutate(lll2018 = suma201704+suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803) %>% 
    mutate(lv2018 =  suma201705+suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804) %>% 
    mutate(v2018 =   suma201706+suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805) %>% 
    mutate(vl2018 =  suma201707+suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806) %>% 
    mutate(vll2018 = suma201708+suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807) %>% 
    mutate(vlll2018 =suma201709+suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808) %>% 
    mutate(lx2018 =  suma201710+suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809) %>% 
    mutate(x2018 =   suma201711+suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810) %>% 
    mutate(xl2018 =  suma201712+suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811) %>% 
    mutate(xll2018 = suma201801+suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812) %>% 
  #       2019            #
  mutate(l2019 =   suma201802+suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901) %>% 
    mutate(ll2019 =  suma201803+suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902) %>% 
    mutate(lll2019 = suma201804+suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903) %>% 
    mutate(lv2019 =  suma201805+suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904) %>% 
    mutate(v2019 =   suma201806+suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905) %>% 
    mutate(vl2019 =  suma201807+suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906) %>% 
    mutate(vll2019 = suma201808+suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907) %>% 
    mutate(vlll2019 =suma201809+suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907+suma201908) %>% 
    mutate(lx2019 =  suma201810+suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907+suma201908+suma201909) %>% 
    mutate(x2019 =   suma201811+suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907+suma201908+suma201909+suma201910) %>% 
    mutate(xl2019 =  suma201812+suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907+suma201908+suma201909+suma201910+suma201911) %>% 
    mutate(xll2019 = suma201901+suma201902+suma201903+suma201904+suma201905+suma201906+suma201907+suma201908+suma201909+suma201910+suma201911+suma201912) 
  
  
  names(mes)
  
corridosmeses<-data.frame(mes[,c(1,134:253)])
write.csv(corridosmeses, file = "corridosmeses.csv")



####################################################################################################################
####################################################################################################################
####################################################################################################################
###############################################TRIMESTRES CORRIDOS##################################################
####################################################################################################################

trim2009 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2009.csv")
trim2009<- data.frame(trim2009[,c(2:6)])
names(trim2009)<- c("DPTO","l2009","ll2009","lll2009","llll2009")

trim2010 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2010.csv")
trim2010<- data.frame(trim2010[,c(2:5)])
names(trim2010)<- c("l2010","ll2010","lll2010","llll2010")

trim2011 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2011.csv")
trim2011<- data.frame(trim2011[,c(2:5)])
names(trim2011)<- c("l2011","ll2011","lll2011","llll2011")

trim2012 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2012.csv")
trim2012<- data.frame(trim2012[,c(2:5)])
names(trim2012)<- c("l2012","ll2012","lll2012","llll2012")

trim2013 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2013.csv")
trim2013<- data.frame(trim2013[,c(2:5)])
names(trim2013)<- c("l2013","ll2013","lll2013","llll2013")

trim2014 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2014.csv")
trim2014<- data.frame(trim2014[,c(2:5)])
names(trim2014)<- c("l2014","ll2014","lll2014","llll2014")

trim2015 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2015.csv")
trim2015<- data.frame(trim2015[,c(2:5)])
names(trim2015)<- c("l2015","ll2015","lll2015","llll2015")

trim2016 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2016.csv")
trim2016<- data.frame(trim2016[,c(2:5)])
names(trim2016)<- c("l2016","ll2016","lll2016","llll2016")

trim2017 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2017.csv")
trim2017<- data.frame(trim2017[,c(2:5)])
names(trim2017)<- c("l2017","ll2017","lll2017","llll2017")

trim2018 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2018.csv")
trim2018<- data.frame(trim2018[,c(2:5)])
names(trim2018)<- c("l2018","ll2018","lll2018","llll2018")

trim2019 <- read.csv("~/Documents/Tesis/Bases de datos/desagregacion trim y anual/trim2019.csv")
trim2019<- data.frame(trim2019[,c(2:5)])
names(trim2019)<- c("l2019","ll2019","lll2019","llll2019")


trimcorrido<- cbind(trim2009,trim2010,trim2011,trim2012,trim2013,trim2014,trim2015,trim2016,trim2017,trim2018,trim2019)





trimes <-trimcorrido %>%    #         2010          #
  mutate(uno2010 =   ll2009+lll2009+llll2009+l2010) %>% 
  mutate(dos2010 = lll2009+llll2009+l2010+ll2010) %>% 
  mutate(tres2010 =llll2009+l2010+ll2010+lll2010) %>% 
  mutate(cuatro2010 =l2010+ll2010+lll2010+llll2010) %>% 
  #         2011          #
  mutate(uno2011 =   ll2010+lll2010+llll2010+l2011) %>% 
  mutate(dos2011 = lll2010+llll2010+l2011+ll2011) %>% 
  mutate(tres2011 =llll2010+l2011+ll2011+lll2011) %>% 
  mutate(cuatro2011 =l2011+ll2011+lll2011+llll2011) %>% 
  #         2012          #
  mutate(uno2012 =   ll2011+lll2011+llll2011+l2012) %>% 
  mutate(dos2012 = lll2011+llll2011+l2012+ll2012) %>% 
  mutate(tres2012 =llll2011+l2012+ll2012+lll2012) %>% 
  mutate(cuatro2012 =l2012+ll2012+lll2012+llll2012) %>% 
  #         2013          #
  mutate(uno2013 =   ll2012+lll2012+llll2012+l2013) %>% 
  mutate(dos2013 = lll2012+llll2012+l2013+ll2013) %>% 
  mutate(tres2013 =llll2012+l2013+ll2013+lll2013) %>% 
  mutate(cuatro2013 =l2013+ll2013+lll2013+llll2013) %>% 
  #         2014          #
  mutate(uno2014 =   ll2013+lll2013+llll2013+l2014) %>% 
  mutate(dos2014 = lll2013+llll2013+l2014+ll2014) %>% 
  mutate(tres2014 =llll2013+l2014+ll2014+lll2014) %>% 
  mutate(cuatro2014 =l2014+ll2014+lll2014+llll2014) %>%
  #         2015          #
  mutate(uno2015 =   ll2014+lll2014+llll2014+l2015) %>% 
  mutate(dos2015 = lll2014+llll2014+l2015+ll2015) %>% 
  mutate(tres2015 =llll2014+l2015+ll2015+lll2015) %>% 
  mutate(cuatro2015 =l2015+ll2015+lll2015+llll2015) %>%
  #         2016          #
  mutate(uno2016 =   ll2015+lll2015+llll2015+l2016) %>% 
  mutate(dos2016 = lll2015+llll2015+l2016+ll2016) %>% 
  mutate(tres2016 =llll2015+l2016+ll2016+lll2016) %>% 
  mutate(cuatro2016 =l2016+ll2016+lll2016+llll2016) %>% 
  #         2017          #
  mutate(uno2017 =   ll2016+lll2016+llll2016+l2017) %>% 
  mutate(dos2017 = lll2016+llll2016+l2017+ll2017) %>% 
  mutate(tres2017 =llll2016+l2017+ll2017+lll2017) %>% 
  mutate(cuatro2017 =l2017+ll2017+lll2017+llll2017) %>% 
  #         2018          #
  mutate(uno2018 =   ll2017+lll2017+llll2017+l2018) %>% 
  mutate(dos2018 = lll2017+llll2017+l2018+ll2018) %>% 
  mutate(tres2018 =llll2017+l2018+ll2018+lll2018) %>% 
  mutate(cuatro2018 =l2018+ll2018+lll2018+llll2018) %>% 
  #         2019          #
  mutate(uno2019 =   ll2018+lll2018+llll2018+l2019) %>% 
  mutate(dos2019 = lll2018+llll2018+l2019+ll2019) %>% 
  mutate(tres2019 =llll2018+l2019+ll2019+lll2019) %>% 
  mutate(cuatro2019 =l2019+ll2019+lll2019+lll2019)
  

names(trimes)

corridostrim<-data.frame(trimes[,c(1,46:85)])
write.csv(corridostrim, file = "corridostrim.csv")



