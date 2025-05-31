getPvalue<-function(A){
  return(shapiro.test(A)$p.value)
}
