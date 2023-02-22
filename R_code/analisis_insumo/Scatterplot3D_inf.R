#3-D scatterplot with vertical lines and point labels
library(scatterplot3d)
library(Rcmdr)
Multiplicadores <- 
  readXL("C:/Users/asus/Dropbox/Documento ponencia/Tabla de multiplicadores.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)
attach(Multiplicadores)
names(Multiplicadores)
with(Multiplicadores, {
  s3d <- scatterplot3d(
    x = MdETipo2,
    y = MdInTipo2,
    z = Multiplicador.de.producto,
    color = "Blue",
    pch = 19,
    type = "h",
    main = "Multiplicadores para la economía boyacense",
    xlab = "Multiplicador de Empleo",
    ylab = "Mutiplicador de Ingreso",
    zlab = "Multiplicador de Producto")
  # convert 3-D coords to 2D projection
  s3d.coords <- s3d$xyz.convert(MdETipo2, MdInTipo2,`Multiplicador de producto`)
  # plot text with 50% shrink and place to right of points
  text(s3d.coords$x,
       s3d.coords$y,
       labels = row.names(Multiplicadores),
       cex = .5,
       pos = 4)
})


eslabonamientos <- readXL("C:/Users/asus/Desktop/eslabonamientos.r.xlsx", rownames=FALSE, 
                          header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)

attach(eslabonamientos)
names(eslabonamientos)
with(eslabonamientos, {
  s3d <- scatterplot3d(
    x = Eslabonmientos.atras,
    y = Eslabonamientos.adelante,
    z = Eslabonamientos.puros,
    color = "Blue",
    pch = 19,
    type = "h",
    main = "Eslabonamientos para la economía boyacense",
    xlab = "Eslabonamientos hacia atras",
    ylab = "Eslabonamientos hacia adelante",
    zlab = "Eslabonamientos puros")
  # convert 3-D coords to 2D projection
  s3d.coords <- s3d$xyz.convert(Eslabonmientos.atras, Eslabonamientos.adelante,Eslabonamientos.puros)
  # plot text with 50% shrink and place to right of points
  text(s3d.coords$x,
       s3d.coords$y,
       labels = row.names(eslabonamientos),
       cex = .5,
       pos = 4)
})




library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)
library(Rcmdr)


encadena <- read_excel("encadena.xlsx")

win.graph()


ggplot(encadena,aes(Encadenamientos_atras, Encadenamientos_adelante,
                    label=encadena$Código)) + geom_text()+geom_vline(xintercept = 95.86429934
                    )+geom_hline( yintercept=95.9331548)+theme (text = element_text(size=12))+labs(x="Eslabonamientos hacia atrás",y="Eslabonamientos hacia adelante")+ 
theme (plot.title = element_text(family="Arial", size=rel(1.5), vjust=3, face="plain",  color="black", 
                                 lineheight=12.5))+theme(plot.title = element_text(hjust = 0.5))+ geom_text(data = NULL, x = 30, y = -26, label = "Sectores Independientes")+ geom_text(data = NULL, x = 350, y = 80, label = "Sectores Impulsores")+ geom_text(data = NULL, x = 46, y = 600, label = "Sectores Base")+ geom_text(data = NULL, x = 350, y = 560, label = "Sectores Clave")


