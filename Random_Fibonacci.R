

f_b<-function(n){
  if(!is.integer(n) & any(c(n<0,n==Inf))){
    return(warning("n must be a Natural number"))
  }
  ini<-c(0,1)
  operacion<-function(k,m_p_,n){
    m_p<-rbinom(1,size=1,prob = 1/2)
    if(n!=0){
      if (m_p_==0) {
        p<-(sum(k))
      }else{
        p<-(k[1]-k[2])
      }
      n<-n-1
      print(c(n,p))
      p<-operacion(c(k[2],p),m_p,n)
    }
    else{
      p<-0
    }
    return(p)
  }
  
  operacion(ini,m_p,n)
  
  
  
  
  
}