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
  
  m_aux <- m2 <- matrix(integer(alto*ancho), alto, ancho)
  for (i in 1:(alto-2)) {
    for (j in 1:(ancho-2)) {
      m2[i+1,j+1] <- m[i,j]
    }
  }
  
  for (i in 2:(alto-1)) {
    for (j in 2:(ancho-1)) {
      # print(c(i,j));m2[i, j] <- m2[i, j] + 1
      # Valoración
      vivos <- c(m2[i-1,j-1] == 1, m2[i,j-1] == 1, m2[i+1,j-1] == 1,
        m2[i-1,j] == 1, m2[i+1,j] == 1,
        m2[i-1,j+1] == 1, m2[i,j+1] == 1, m2[i+1,j+1] == 1)
      
      #Si est{a viva y tiene dos o tres vecinas vivas, sigue viva
      if(m2[i,j] == 1 & sum(vivos) %in% c(2,3)){
        m_aux[i,j] = 1
      }else if(m2[i,j] == 0 & sum(vivos) == 3){
        #Si tiene exactamente 3 vecinas vivas revive
        m_aux[i,j] = 1
      }else{
        #Otro caso muere
        m_aux[i,j] = 0
      }
    }
  }
  return(m_aux[-c(1,len),-c(1,len)])
}
