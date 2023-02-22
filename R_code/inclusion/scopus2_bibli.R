##############################################################################
###Analisis de bibliometria#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
library("dplyr")
library("Matrix")
library("stringr")
library("igraph")
library("FactoMineR")
library("factoextra")
library("rscopus")
library("SnowballC")
library("ggplot2")
library("bibliometrix") ### load bibliometrix package
library("devtools")
##Leer archivos .bib
soc_sciences <- readFiles("SOCI_2010_2011_2012.bib","SOCI_2013_2014.bib","SOCI_2015.bib")
#Convertir a dataframe
soc_sciences_nacional <- convert2df(soc_sciences,dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(soc_sciences_nacional, sep = ";")
#Quitar duplicados
duplicatedMatching(soc_sciences_nacional, Field = "AU", tol = 0.95)
#Red de autor?a
NetMatrix <- biblioNetwork(soc_sciences_nacional, analysis = "collaboration", network = "authors", sep = ";")
NetMatrix2 <- biblioNetwork(soc_sciences_nacional, analysis = "co-occurrences", network = "keywords", sep = ";")
NetMatrix_Uni <- biblioNetwork(soc_sciences_nacional, analysis = "collaboration", network = "universities", sep = ";")
NetMatrix_Coun <- biblioNetwork(soc_sciences_nacional, analysis = "coupling", network = "countries", sep = ";")
# Plot the network
net_auth <- networkPlot(NetMatrix,n=10, type = "kamada", Title = "Collaboration Network")
net_keyw <- networkPlot(NetMatrix2, n=25, type = "kamada", Title = "Keywords Network", labelsize = 0.6)
net_uni <- networkPlot(NetMatrix_Uni, n=15, type = "kamada", Title ="Collaboration Network" )
