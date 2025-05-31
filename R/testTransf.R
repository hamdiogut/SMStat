testTransf<-function(A,B){
  if((getPvalue(A)<0.05) & (getPvalue(B)>0.05)) {
    library(rcompanion)
    k<-transformTukey(A)
    lmbd<-transformTukey(A,returnLambda = TRUE)
    Tr_B<-transformB(B,lmbd[[1]])
    tData<-data.frame(k,Tr_B)
    return(tData)
  } else if((getPvalue(A)>0.05) & (getPvalue(B)<0.05)) {
    library(rcompanion)
    k<-transformTukey(B)
    lmbd<-transformTukey(B,returnLambda = TRUE)
    Tr_B<-transformB(A,lmbd[[1]])
    tData<-data.frame(Tr_B,k)
    return(tData)
  }
  else if((getPvalue(A)<0.05) & (getPvalue(B)<0.05)) {
    library(rcompanion)
    k<-transformTukey(B)
    lmbd<-transformTukey(B,returnLambda = TRUE)
    Tr_B<-transformB(A,lmbd[[1]])
    tData<-data.frame(Tr_B,k)
    return(tData)
  }
}
