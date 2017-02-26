
mape<-function(real,predict){

  difference<-(real-predict)
  difference.abs<-abs(difference)
  par<- difference.abs/real
  difference.par<-sum(par)
  MaPe<-difference.par*100/length(difference)
  return(MaPe)

}
