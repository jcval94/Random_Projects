#RECURSSION
library(purrr)

##Ejemplo

#Al ultimo resultado vuelve a aplicar
#una función recursiva

#Pegar el nuevo valor
accumulate(letters,paste)

#Multiplicar por 2
accumulate(1:10,~.*2,.init=1)

#A cada uno por separado
map_dbl(1:10,~.*2)

#Ejemplo con una fila

fila<- c(1,3,3,1)

#La función ganadora es:
fila
c(0,fila) + c(fila,0)

#Entonces, apliquemoslo
accumulate(1:10,~c(0,.) + c(.,0))

#Función en una línea de código

pascal<-function(n)
  purrr::accumulate(1:n, ~c(0, .) + c(., 0))

pascal(15)
