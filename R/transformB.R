transformB<-function(B,l){
  if (l>0){TRANS = B^l}
  if (l==0){TRANS = log(B)}
  if (l< 0){TRANS = -1*(B^l)}
  return(TRANS)
}
