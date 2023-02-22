##############################################################################
###Analisis de bibliometria#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
# IMPORTANTE http://bibliometrix.org/documents/bibliometrix_Report.html
install.packages("dplyr")      #TENER CUIDADO A LA HORA DE FILTRAR LOS DATOS EN WOS SE REALICE EN "REGISTRO COMPLETO" Y EN DIFERENTES A?OS PARA CAUNDO SE FILTRE NO EXISTA PROBLEMAS, el a?o 2018 lo rechaza 
install.packages("Matrix")    #
install.packages("stringr")
install.packages("igraph")
install.packages("FactorMiner")
install.packages("bibliometrix", dependencies=TRUE)
library("dplyr")
library("Matrix")
library("stringr")
library("igraph")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("bibliometrix") ### load bibliometrix package
setwd("~/Documents/Investigacion/Inclusion en Tunja")
uptc <- readFiles("savedrecs (1).bib","savedrecs (2).bib","savedrecs (3).bib","savedrecs (4).bib")#sumatoria de los documentos recoopilados






UPTC <- convert2df(uptc, dbsource = "isi", format = "bibtex")
#results <- biblioAnalysis(UPTC, sep = ";")
#NetMatrix <- biblioNetwork(UPTC, analysis = "co-occurrences", network = "keywords", sep = ";")
M<-UPTC





#################
###############CODIGOS DEL LINK 1
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:20])
results <- biblioAnalysis(M)
summary(results, k=10, pause=F, width=130)

plot(x=results, k=10, pause=F)
#CR <- citations(M, field = "article", sep = ";")
#cbind(CR$Cited[1:20])
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
M=metaTagExtraction(M,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)

netstat <- networkStat(NetMatrix)##demorado
summary(netstat,k=10)

histResults <- histNetwork(M, min.citations=quantile(M$TC,0.75), sep = ";")

options(width = 130)
net <- histPlot(histResults, n=20, size = 5, labelsize = 3)

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=3,label.cex=TRUE,label.n=30,edges.min=2)
#Anterior grafica tener encuenta
netstat <- networkStat(NetMatrix)
summary(netstat,k=10)
######
CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=100, clust=5, stemming=FALSE, labelsize=8,documents=20)
#cs=min Degree10
###IMPORTANTE
Map=thematicMap(M, field = "ID", n = 250, minfreq = 5,
                stemming = FALSE, size = 0.5, n.labels=3, repel = TRUE)
plot(Map$map)















############################################
###########################################



CS <- conceptualStructure(UPTC,field="ID", minDegree=20, k.max=21, stemming=FALSE)



M<-metaTagExtraction(UPTC, Field = "AU_CO", sep = ";")
NetMatrix<-biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 20, Title = "Country Collaboration", type = "circle", size=TRUE,remove.multiple=FALSE)
net=networkPlot(NetMatrix, n = 50, Title = "Country Collaboration", type = "fruchterman", size=FALSE, remove.multiple=TRUE, vos.path="C:/Users/Adriana.Diaz/Documents/Alonso/Observatorio/bibliometRics/VOSviewer",weighted=TRUE)
#Thematic map
data(UPTC)
years=c(2011)
list_df=timeslice(UPTC, breaks = years)
M1=list_df[[1]]
M2=list_df[[2]]
NetMatrix1 <- biblioNetwork(M1, analysis = "co-occurrences",
                            network = "keywords", sep = ";")
S1 <- normalizeSimilarity(NetMatrix1, type = "association")
net1 <- networkPlot(NetMatrix1, normalize = "association",n = 50,
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res1 <- thematicMap(net1, NetMatrix1, S1)
plot(res1$map)
NetMatrix2 <- biblioNetwork(M2, analysis = "co-occurrences",
                            network = "keywords", sep = ";")
S2 <- normalizeSimilarity(NetMatrix2, type = "association")
net2 <- networkPlot(NetMatrix2, normalize = "association",n = 50,
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res2 <- thematicMap(net2, NetMatrix2, S2)
plot(res2$map)
nexus <- thematicEvolution(res1,res2,weighted=FALSE)
