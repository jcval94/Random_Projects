#Life game

#Una celula muerta con 3 vecinas vivas revive
#una celula con 2 o más vecinas vivas sigue viva
#Caso contrario muere

n <- 5
tablero <- matrix(integer(n^2), n, n)

Iter<-function(m)
{
  len <- sqrt(length(m))
  m2<-matrix(integer(len^2), len, len)
  for (i in 1:length(len)) {
    for (j in 1:length(len)) {
      if(m[i,j] == 0) {
        #revivir
        rededorh<-sapply(c(i-1, i, i+1),
                         function(x) min(max(x,1),len))
        rededorh<-rededorh[!duplicated(rededorh)]
        rededorw<-sapply(c(j-1, j, j+1),
                         function(x) min(max(x,1),len))
        rededorw<-rededorw[!duplicated(rededorw)]
        expand.grid(rededorh, rededorw)
      }else{
        #seguir vivo
        #morir
      }
    }
  }
}
