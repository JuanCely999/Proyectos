##############################################################################
###Analisis de bibliometria#
######################Autor: Juan Pablo Cely#################################
###############################19-03-2020####################################
#
install.packages("dplyr")
install.packages("Matrix")
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
uptc <- readFiles("scopus.bib")
UPTC <- convert2df(uptc, dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(UPTC, sep = ";")
NetMatrix <- biblioNetwork(UPTC, analysis = "co-occurrences", network = "keywords", sep = ";")

CS <- conceptualStructure(UPTC,field="ID", minDegree=4, k.max=5, stemming=FALSE)



M<-metaTagExtraction(UPTC, Field = "AU_CO", sep = ";")
NetMatrix<-biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 20, Title = "Country Collaboration", type = "circle", size=TRUE,remove.multiple=FALSE)
net=networkPlot(NetMatrix, n = 50, Title = "Country Collaboration", type = "fruchterman", size=FALSE, remove.multiple=TRUE, vos.path="C:/Users/Adriana.Diaz/Documents/Alonso/Observatorio/bibliometRics/VOSviewer",weighted=TRUE)
#Thematic map
data(UPTC)
years=c(2016)
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
