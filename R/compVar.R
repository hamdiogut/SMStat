compVar<-function(A,B){
  if(var.test(A,B)$p.value>0.05){
    return(TRUE)
  } else {return(FALSE)}
}
