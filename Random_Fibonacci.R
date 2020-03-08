

f_b<-function(n){
  if(!is.integer(n) & n<0){
    return(warning("n must be a Natural number"))
  }
  m_p<-rbinom(n,size=1,prob = 1/2)
  ini<-c(0,1)
  operacion<-function(k,m_p_,n){
    n<-n-1
    if (m_p_==0) {
      return(sum(k))
    }else{
      return(k[1]-k[2])
    }
  }
  
  
}