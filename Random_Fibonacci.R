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

##Simulate 100 fibos
fibos_l<-list()
for (k in 1:100) {
  print(k)
  l1<-f_b(n)
  fibos_l[[k]]<-(purrr::map_df(l1,~data.frame(.x[1],.x[2])))
  names(fibos_l[[k]])<-c("n","suma")
}

fibos_l2<-do.call(rbind,fibos_l)

fibos_l2[["Simul"]]<-as.factor(rep(1:100,each=n))

#Max value is != Imf
summary(fibos_l2[[2]])

#Veamos las primeras 5 líneas
fibos_l2[1:(n*5),] %>% 
  ggplot(aes(n,abs(suma),color=Simul))+
  geom_line()+
  scale_y_log10()

#Obtengamos nuestro número dorado...

div<-c(1,fibos_l2[[2]])/c(fibos_l2[[2]],1)

Reales<-div[!(div %in% c(Inf,-Inf) |abs(div) > 10^40)]

#Que densidad tan alocada
R_a<-abs(Reales)
DR_a<-density(R_a)
plot(DR_a)

y_D<-DR_a[["y"]]
maxi<-DR_a[["x"]][y_D==max(y_D)]

abline(v = maxi, col="red", lwd=3, lty=2)

maxi*2

