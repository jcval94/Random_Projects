#Life game

#Una celula muerta con 3 vecinas vivas revive
#una celula con 2 o más vecinas vivas sigue viva
#Caso contrario muere

n <- 5
tablero <- matrix(integer(n^2), n, n)

#No hay diferencia en lógica si aumento una dimesión más la matriz
#y solo muestro lo que sucede en n-1
Iter<-function(m)
{
  alto <- dim(m)[1] + 2
  ancho <- dim(m)[2] + 2
  
  m2 <- matrix(integer(alto*ancho), alto, ancho)
  for (i in 2:(alto-1)) {
    for (j in 2:(ancho-1)) {
      # print(c(i,j))
      # Reglas
        ##m2[i, j] <- m2[i, j] + 1
        
        
      
    }
  }
  return(m2[-c(1,len),-c(1,len)])
}
