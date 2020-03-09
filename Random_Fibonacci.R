#https://www.youtube.com/watch?v=ELA8gNNMHoU

f_b<-function(N){
  if(!is.integer(N) & any(c(N<0,N==Inf))){
    return(warning("N must be a Natural number"))
  }
  ini<-c(0,1)
  acumulado<-acumul<-vector(mode = "list", length = N)
  
  operacion<-function(k,n,acumul){
    #if(missing(acumul)){acumul<-acumulado}
    m_p_<-rbinom(1,size=1,prob = 1/2)
    if(n!=0){
      if (m_p_==0) {
        p<-(sum(k))
      }else{
        p<-(k[1]-k[2])
      }
      iter<-N-n+1
      print(c(iter,p))
      acumul[[iter]]<-c(iter,p)
      n<-n-1
      p<-operacion(c(k[2],p),n,acumul)
      print(paste0("A",acumul[[iter]]))
    }else{
      p<-0
    }
    return(acumul)
  }
  
  seee<-operacion(k=ini,n=N,acumul = acumulado)
  return(acumul)
}

  
  
n<-1000

l1<-f_b(n)


