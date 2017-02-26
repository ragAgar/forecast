arimaMAPE<-function(p,d,q,y,s=0,jissoku,trace="T",method="MAPE",h=20){
  bestmape<-array(1:h)
 for(h1 in 1:h){
  jissokutemp<-jissoku[1:h1]
  best<-0
  best1<-0
  best2<-0
#  print(h1)
   for (i in 0:p){
     for (j in 0:d){
       for(k in 0:q){
         for(P in 0:p){
           for(D in 0:d){
             for(Q in 0:q){
            
              res<-try(model<-arima(y,order=c(i,j,k),seasonal=list(order=c(P,D,Q),period=s)),silent=T)
              if((class(res)=="try-error")&(trace=="T")){ next}
              fc.arima <- forecast(model,h=h1)
              pred.arima <- as.vector(fc.arima$mean)
              
              difference <- (pred.arima)-(jissokutemp)
              difference.positive <- sum(difference[difference >= 0])
              difference.negative <- -(sum(difference[difference <= 0]))
              absolute<-abs(difference)
              differenceabs<-sum(absolute)
              ratio.positive <- (difference.positive)/(differenceabs)
              ratio.negative <- (difference.negative)/(differenceabs)

              if((method=="MAPE")){
                mape=(sum((absolute/jissoku)*100))/length(absolute)
                if(best==0){
                  best<-mape
                  besti<-i
                  bestj<-j
                  bestk<-k
                  bestP<-P
                  bestD<-D
                  bestQ<-Q
                }
                else if((best!=0)&(best>=mape)){
                  best<-mape
                  besti<-i
                  bestj<-j
                  bestk<-k
                  bestP<-P
                  bestD<-D
                  bestQ<-Q
                  }}}}}}}}
                  
                  bestmape[h1]<-best
                  cat("\n","The best model(predict span is ",h1,") is (",besti,bestj,bestk,")(",bestP,bestD,bestQ,")[",s,"]","\n")
                  cat("   ",method," = ",best,"\n")
                  bestmodel<-arima(y,order=c(besti,bestj,bestk),seasonal=list(order=c(bestP,bestD,bestQ),period=s))
                  fc.arima <- forecast(bestmodel,h=h1)
                  pred.arima <- as.vector(fc.arima$mean)
                  difference <- (pred.arima)-(jissokutemp)
                  absolute<-abs(difference)
                  difference.positive <- sum(difference[difference >= 0])
                  difference.negative <- -(sum(difference[difference <= 0]))
                  differenceabs<-sum(absolute)
                  ratio.positive <- (difference.positive)/(differenceabs)
                  ratio.negative <- (difference.negative)/(differenceabs)
                  cat("    MIC(+,-)   = ",ratio.positive,",",ratio.negative,"\n")
                  cat("##########################################################","\n")

 }
 return(bestmape)
 }