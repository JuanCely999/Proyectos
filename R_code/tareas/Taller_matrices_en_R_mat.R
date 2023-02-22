#############################################################################
###Taller matrices en R#####################################
######################Autor: Juan Pablo Cely#################################
###############################27-06-2020####################################
#Link
#https://rpubs.com/Cesar_AHN/operaciones_con_matrices_multiplicacion_determinante_inversa_factorizacion_descomposicion_grafica
rm(list=ls())
#1. Determinantes
# Para definir la matriz A:
a<-c(30,45,55)       # El primer vector.
b<-c(32,40,70)       # El segundo vector.
d<-c(25,15,68)       # El tercer vector.

A<-rbind(a,b,d)    # La matriz A.
A
det(A)

# Para definir la matriz B:
a2<-c(2,5,7)       # El primer vector.
b2<-c(12,4,1)       # El segundo vector.
d2<-c(8,-5,-3)       # El tercer vector.

B<-rbind(a2,b2,d2)    # La matriz B.
B
det(B)


# Para definir la matriz C:
a3<-c(-5,6,0)       # El primer vector.
b3<-c(9,12,3)       # El segundo vector.
d3<-c(8,-2,5)       # El tercer vector.

C<-rbind(a3,b3,d3)    # La matriz B.
C
det(C)

# Para definir la matriz D:
a4<-c(-1,6,1)       # El primer vector.
b4<-c(9,7,3)       # El segundo vector.
d4<-c(-4,-2,2)       # El tercer vector.

D<-rbind(a4,b4,d4)    # La matriz D.
D
det(D)


# Para definir la matriz E:
a5<-c(1,4,3)       # El primer vector.
b5<-c(2,2,70)       # El segundo vector.
d5<-c(-7,15,-4)       # El tercer vector.

E<-rbind(a5,b5,d5)    # La matriz E.
E
det(E)


# Para definir la matriz F:
a6<-c(7,9,12)       # El primer vector.
b6<-c(3,11,6)       # El segundo vector.
d6<-c(4,-7,9)       # El tercer vector.

F<-rbind(a6,b6,d6)    # La matriz F.
F
det(F)


# Para definir la matriz G:
a7<-c(5,5,9)       # El primer vector.
b7<-c(7,12,7)       # El segundo vector.
d7<-c(5,14,-5)       # El tercer vector.

G<-rbind(a7,b7,d7)    # La matriz G.
G
det(G)

  
# Para definir la matriz H:
a8<-c(8,-2,5)       # El primer vector.
b8<-c(9,7,3)       # El segundo vector.
d8<-c(-9,9,4)       # El tercer vector.

H<-rbind(a8,b8,d8)    # La matriz H.
H
det(H)

#2. Operaciones
A21<- (3*(A-C))+6
A21#//

A22<-A+B-F
A22#//

A23<- (2*A)-(0.5*(G+H))
A23#//

A24<-(5*(C+G))
A24#//

A25<-(3*C)-(2*B)
A25#//

A26<- 0.25*(A-C)
A26#//





#----------------------------------
#3. Martriz inversa
rm(list=ls())
# Para definir la matriz A:
a<-c(0.4,0.6,-0.3)       # El primer vector.
b<-c(0.2,0.1,-0.1)       # El segundo vector.
d<-c(0.3,0.2,-0.4)       # El tercer vector.

A<-rbind(a,b,d)    # La matriz A.
solve(A)

# Para definir la matriz B:
a2<-c(1,2,1)       # El primer vector.
b2<-c(2,5,2)       # El segundo vector.
d2<-c(-1,-2,2)       # El tercer vector.

B<-rbind(a2,b2,d2)    # La matriz B.
solve(B)


# Para definir la matriz E:
a3<-c(2,6,-3)       # El primer vector.
b3<-c(4,8,9)       # El segundo vector.
d3<-c(-7,2,5)       # El tercer vector.

E<-rbind(a3,b3,d3)    # La matriz B.
solve(E)

# Para definir la matriz F:
a4<-c(-5,4,-3)       # El primer vector.
b4<-c(10,-7,6)       # El segundo vector.
d4<-c(8,-6,5)       # El tercer vector.

F<-rbind(a4,b4,d4)    # La matriz D.
solve(F)



#----------------------------------
#3. Problema
rm(list=ls())
# Para definir la matriz A:
a<-c(20,30,25)       # El primer vector.
b<-c(8,10,6)       # El segundo vector.


# Para definir la matriz B:
a2<-c(22,36,24)       # El primer vector.
b2<-c(9,9,8)       # El segundo vector.


# Para definir la matriz C:
a3<-c(18,32,26)       # El primer vector.
b3<-c(11,8,5)       # El segundo vector.


cmater<-a+a2+a3
ctrans<-b2+b3


cmater
ctrans

