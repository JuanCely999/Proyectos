###########################################################################################
# CURSO R STUDIO NOVIEMBRE 8 2019. UPTC FACULTAD DE CIENCIAS ECON?MICAS Y ADMINISTRATIVAS #
#                              Juan Pablo Cely                                           #
#                                    19-03-2020                                           #
##############################################################################
#CURSO R STUDIO NOVIEMBRE 8 2019. UPTC FACULTAD DE CIENCIAS ECON?MICAS Y ADMINISTRATIVAS 

5+9+2
5/3
5%/%3  #Division entera
5%%3    #residuo de la division
sqrt(78) #raiz cuadrada
pi #número pi
abs(-6)  #Valor absoluto
factorial(5) #factorial de un numero
choose(8,4)   # 4 combinado 2
round(1/3,2)  # redondea 1.3333333 a dos decimales
5^3
cos(pi/2)
#CREACIÓN DE VARIABLES

a1<-8 #asignar 8 a la varaible a1
(a2<-9)  #asigna el 9 a a2 y la muestra
a1*10
rm(a1)    #ELIMINA el objeto a1
help(sqrt)  #R da informaci?n de la funcion sqrt

#VECTORES

edad<-c(18,25,36,15,14) #se crea un vector de edades
peso<-c(58,64,72,54,51)
traspuesto<-t(peso)

suma<-edad+peso
producto<-edad*peso

edad1<-sort(edad) #ordena edad de menor a mayor

edad2<-sort(edad,decreasing = T)  #ordena edad de mayor a menor
nombres<-c('Carlos','Nancy','Lina','Olga','Julio') #vector con nombres
datos<-data.frame(edad,peso,nombres) #crea una base de datos con edad peso nombres

altura<-c(165,162,168)
altura<-c(altura,160,160) #adicionar datos al vector ALTURA
datos<-data.frame(datos,altura) #agregar altura a la base  datos

#CREAR SECUENCIAS
y<-rep(5,6)  #repite el 5 seis veces
z<-rep(c(7,2,13),4) #repite la secuencia 7,2,13 cuatro veces
w<-rep(c(1,2),c(5,3))  #repite el 1 cinco veces y despues repite el 2 tres veces
t1<-seq(7,29,3) #secuencia iniciando en 7 aumentando de a tres hasta 29
t2<-seq(2008,2020) #secuencia de 2008 a 2020 de uno en uno
t3<-seq(2019,2005,-2) #secuencia inversa de 2019 a 2005 de dos en dos
t4<-56:62   #secuencia automática de uno en uno desde 56 hasta 62
t5<-200:194  #secuencia automática inversa de uno en uno desde 200 hasta 194
t6<-rep(1:4,rep(9,4))

#LETRAS
d<-letters[2:6] #vector con las letras de b a la f

d1<-LETTERS[2:6] #vector con las letras de B a la F  
d2<-letters[-(5:20)] # vector con las sin las letras de la e a la t
d3<-letters[c(5,10,20,25)] #vector con las letras e,j,t,y

#OPERADOR INDICE
p<-1:26  #vector con los numeros del 1 al 26
q<-p<10  #vector que selecciona los elementos de p que cumplen la condicion

d4<-LETTERS[p>20]  #Letras de la 20 a la 26
d5<-letters[p>12 & p<18] #letras de la m a la q

#  FUNCION WHICH

#ejemplo sobre la tabla datos

altos<-which(datos$altura>160) #presenta el indice de los datos que cumplen con la condición

altos1<-datos[which(datos$altura>160),] #tabla con la informacion de datos que cumple la condición
altos2<-datos[which(datos$altura>160 & datos$edad<30 ),]  #tabla con la información de datos que cumple la condición
peso1<-datos[which(datos$peso!=58),] #simbolo de diferente (!=)

datos1<-expand.grid(camisa=c("Amarilla","Verde","Azul"), pantalon=c("Negro", "Gris", "Rojo"))
datos1a<-expand.grid(c(3,4),c(7,8,9),c(4,2),c(0))
datos1a
datos2a<-expand.grid(c(3,4),c(7,8,9),c(4,2),c(0,7,8,3,1))
datos2a


#####################################################
####################MATRICES##########################
m1<-matrix(0,2,3)#Listado de numeros, matriz de 2*3 con ceros
m2<-matrix(c(1,3,4,5),4,2)#4*2 c() Por defecto es en columna 
m21<-matrix(c(1,3,4,5),4,2,byrow = T)#Entra por fila
dim(m2)#Tama?o de la matrix
m2t<-t(m2)#Matriz transpuesta
m2d<-diag(m2)#Extrae los elementos de la diagonal
m3<-matrix(seq(7,59,3),3,6)
sumam3<-sum(m3)#suma de los datos
sumam4<-sum(m3,m2)
sumam5
producto<-prod(m3)#Multiplicar la matriz
m5<-matrix((2:13),3)#matriz con tres filas
m6<-matrix((2:13),,3)#matriz con tres columnas
m7<-matrix(5:7,6,3)

#####No todas las matrices se pueden multiplicar
###A_n*m   B_m*p = C_n*p
m5m6<-m5%*%m6#multiplicar matrices
rownames(m5m6)<-letters[8:10]#Coloca nombres en filas
colnames(m5m6)<-c("primero","segundo","tercero")#Coloca columnas
m5m6
m7<-matrix(c(3,5,1,2,0,1,4,2,-1),3)#matriz con 3 filas
determinante<-det(m7)
###### No se puede calcular el determinante para matrices no cuadradas

inversam7<-solve(m7)
m7%*%inversam7
round(m7%*%inversam7,4)#Redondear la matrix en 4 cifras
b<-c(-8,0,10)#vector b
solucion<-solve(m7,b)#####Revisar cuaderno explicacion de matrices

######################################
#############Arreglos(Array)
#Un array es una matriz multidimensional
arreglo1<-array(12:16,2)#Arreglo que toma los 2 primeros numeros
arreglo2<-array(12:16,4)#Arreglo que toma los 4 primeros numeros
arreglo3<-array(12:16,9)#Arreglo que toma los 9 primeros numeros

arreglo4<-array(20:35,c(3,2,4))#Coloca los numeros del 20 al 35 en un arreglo

#######################################
########Muestras aleatorias
ciudad<-c("Tunja","Sogamoso","Duitama","Paipa","Tuta","Iza","Nobsa")#Vectores
muestra1<-sample(ciudad,4)
muestra1a<-sample(ciudad,4)

muestra2<-sample(ciudad,4, replace=T)#Seleccionar una muestra con reemplazo
muestra2a<-sample(ciudad,12, replace=T)
probab<-c(0.1,0.8,0.05,0.15,0.02,0.25,0.35)#Vector de ponderaciones
sum(probab)#halla la suma de los elementos del vector
muestra4<-sample(ciudad,14,prob=probab)


