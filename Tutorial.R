#Todo en R s un objeto y se dividen en:
#Matrices
array() 
#Lista
list()
#Tablas
data.frame()
#vectores
c()
#Funciones
function()
#Y también se ueden crear nuevos


1.- Asugnación de un valor

x<-1
y<-2

x+y


Una nueva asignación reemplaza a la anterior
3->x
4->y

x+y


#####Conocer el entorno de RStudio
##########################

----------------------------
Los objetos seguardan en un archivo te tipo RData llamado worksace

El contenido se ve con 
ls()

Para eliminar un objeto del workspace se utiliza el comando
rm("x")

Veamos nuevamente el workspace
ls()

Para borrar todo utilizaremos

rm(list=ls())

También se uede guardar el historial de ejecución (se ve apretando la fecha de arriba)

#Ejercicio:
#dale nuevos valores a x y y


-----------------------------
Operacones

2/3

Parte entera
12//5

2+3
3*2
3^4
1-3

Inf+Inf

0^2

sin(0)

cos(0)

#Suma (Sigma)
sum(2,5)

#Producto (pi)
prod(4,5)


#escribe la fórmula del teorema de pitágoras


-----------------------------
Vectores (se utiliza la letra c de concatenar)

vec<-c(0,1,2,3,4)

vec2<-0:4

Todo se ve afectado
vec+1
vec*4
vec^2

#Residuo
10%%3

#Si tienen la misma longitud la operación se hace entrada por entrada
vec-vec2

sum(vec)
prod(vec)

sino, se "repite"

vec+c(2,3)
#Otros datos

mean()
median()
var()
sd()

summary()

#Cuál es el residuo de ma media entre 0 y 100 y 7
-----------------------------
#Añadir texto

vec<-c("Hola","perro")
vec

#Agregar un valor

vec<-c(vec,"xD")
vec

#Crea 4 vectores con números y caracteres

-----------------------------

Operadores lógicos
Igualdad
1==4

2<4

7>=8

2>-2 & 3<8

"Hola" == "hola" | 4<8

Vec_logico<-c("Hola" == "hola", 4<8)

#Algúno es vergadero
any(Vec_logico)

#Todos son verdaderos
all(Vec_logico)

#También existe

2 %in% c(1,5,2,6)

#Negación

!2 %in% c(1,5,2,6)


-----------------------------
#Selección de elementos

x<-1:9

y<-rep(4,7)

y1<-rep(4:7,times=5)

z<-rep(1:4,each=2)

#Se pueden escribir el nombre de los parámetros
z1<-seq(from=1,to=30,by=5)

z2<-seq(from=1,to=30,length=5)

#Longitud de un vector
length(y)


###Se pueden nombrar los vectores
###EJERCICIOS
#Crea la siguiente lista: pares del 0 al 15 y unirlos con impares del 9 al 32 (repetida 5 veces)

-----------------------------
#Se utilizan para tener un mejor almacenamiento en memoria
#Factores
f_y1<-factor(y1)
levels(f_y1)<-c("coca","pepsi","fanta","manzanita")
f_y1

###EJERCICIOS
#Transforma todos los vectores a factores

-----------------------------
#Posicionamiento 

y1[1]

#Tal que

y1[y1>5]

#Funciona para listas, vectores y data frames
y1[!c(y1>5 & y1<7)]
y1[!c(y1>5 & y1<7)]

#Reasignación
y1[1]<-9

max(y1)
y1[1] <- -3

min(y1)

y1[y1>5 & y1<7]<-8

#Traducir: Los X en los naturales tal que X es mayor o igual a 7, menor a 100 y distinto de 36

-----------------------------
#Listas
#Son unidades de almacenamiento (pueden almacenar cualquier otro tipo de estructura)
#Un vector no puede almacenar un data.frame

K<-list()


#Se pueden agregar nombres a los elementos de la lista




#Ubicación en una lista


-----------------------------
#Data Frame (una tabla)

df<-data.frame(Nombres=c("Paco","Joshua Hermoso"),Edad=c(24,25))

#La consulta se hace igual al de una lista
df$Nombre

#Convertimos nuestras columnas en vectores
attach(df)


#Trasponer
t(df)


####EJERCICIOS
#Crea una df con peso y estatura aproximada de 3 de los Pacos

-----------------------------

#Objetos internos

#Muestra los que hay internamente
data()

#R ya tiene algunos data frames precargados, se invocan con el comando data()
data(iris)

#Contienen datos, funciones, etc
library(tidyverse)

#Instalar una librería
install.packages("tidyverse")
###EJERCICIOS
#Instala el paquete FitUltD

-----------------------------

#funciones

f_xy<-function(x,y){
	resp<-x+2*y-sqrt(7x)
	return(resp)
}
f_xy(1,2)

f_xy<-function(x,y){
	resp<<-x+2*y-sqrt(7x)
	#Podemos hacer que no regrese nada
	return(invisible())
}

resp

###EJERCICIOS
#Crea una función que si yo le asigno un vector me regrese los dígitos pares de ese vector
