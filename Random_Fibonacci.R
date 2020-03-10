#https://www.youtube.com/watch?v=ELA8gNNMHoU

f_b<-function(N){
  if(!is.integer(N) & any(c(N<0,N==Inf))){
    return(warning("N must be a Natural number"))
  }
  ini<-c(0,1)
  acumulado<-acumul<-vector(mode = "list", length = N)
  
  operacion<-function(k,n,acumul){
    m_p_<-rbinom(1,size=1,prob = 1/2)
    if(n!=0){
      if (m_p_==0) {
        p<-(sum(k))
      }else{
        p<-(k[1]-k[2])
      }
      iter<-N-n+1
      acumul[[iter]]<-c(iter,p)
      n<-n-1
      #print(paste0("A",acumul[[iter]]))
      acumul<-operacion(c(k[2],p),n,acumul)
      return(acumul)
      
    }else{
      return(acumul)
    }
  }
  seee<-operacion(k=ini,n=N,acumul = acumulado)
  return(seee)
}

  
n<-1000

l1<-f_b(n)

library(tidyverse)


fibo<-(purrr::map_df(l1,~data.frame(.x[1],.x[2])))

names(fibo)<-c("n","suma")

fibo %>% ggplot(aes(abs(suma),n))+geom_line()+scale_x_log10()

