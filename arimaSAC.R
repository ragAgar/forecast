arimaSAC<-function(p,d,q,y,s="MONTH",jissoku,trace="T",method="MAPE",h=2){
  if(s=="MONTH"){s<-12}
  if(s=="WEEK"){s<-52}
  th<-length(jissoku)
  traintemp<-y[1:(length(y)-(h+1))]
  evaluationtemp<-rbind(y[(length(y)-(h-1)):length(y)],jissoku)
  pred.value<-array(rep(0,(th)))
  for(h1 in 1:th){
    best<-0
    evaluation<-evaluationtemp[-(1:h1)]
    train<-rbind(traintemp,evaluationtemp[1:h1])
  for(s1 in 0:s){
    if(s==12){if(!((s1==0)|(s1==6)|(s1==12))==T){next}}
    if(s==52){if(!((s1==0)|(s1==26)|(s1==52))==T){next}}
  #  print(s1)
    for (i in 0:p){
        for (j in 0:d){
            for(k in 0:q){
                for(P in 0:p){
                    for(D in 0:d){
                        for(Q in 0:q){
                            res<-try(model<-arima(train,order=c(i,j,k),seasonal=list(order=c(P,D,Q),period=s1)),silent=T)
                            if((class(res)=="try-error")&(trace=="T")){next}
                            fc.arima <- forecast(model,h=h)
                            pred.arima <- as.vector(fc.arima$mean)
                            if(min(pred.arima)<0){next}#負の予想があるモデルははじく
                            difference <- (pred.arima[1:(h-1)])-(evaluation[1:(h-1)])
                            difference.positive <- sum(difference[difference >= 0])
                            difference.negative <- -(sum(difference[difference <= 0]))
				              			absolute<-abs(difference)
							              differenceabs<-sum(absolute)
					              		ratio.positive <- (difference.positive)/(differenceabs)
                            ratio.negative <- (difference.negative)/(differenceabs)

                            mape=(sum((absolute/evaluation[1:(h-1)])))*100/length(absolute)
　#　　　　　　　　　　　　　　　　cat(i,j,k,P,D,Q,s1,mape,"\n")
                            if(best==0){
                                  best<-mape
                                  besti<-i
                                  bestj<-j
                                  bestk<-k
                                  bestP<-P
                                  bestD<-D
                                  bestQ<-Q
                                  bests<-s1
                                    }

                            else if((!(best==0))&(best>=mape)){
                                  best<-mape
                                  besti<-i
                                  bestj<-j
                                  bestk<-k
                                  bestP<-P
                                  bestD<-D
                                  bestQ<-Q
                                  bests<-s1
                                    }
                                  }}}}}}}


          cat("\n","The best model is  (",besti,bestj,bestk,")(",bestP,bestD,bestQ,")[",bests,"]","\n")

          bestmodel<-arima(train,order=c(besti,bestj,bestk),seasonal=list(order=c(bestP,bestD,bestQ),period=bests))
          fc.arima <- forecast(bestmodel,h=h)
          pred.arima <- as.vector(fc.arima$mean)
          difference <- (pred.arima[1:(h-1)])-(evaluation[1:(h-1)])
          difference.positive <- sum(difference[difference >= 0])
          difference.negative <- -(sum(difference[difference <= 0]))
          absolute<-abs(difference)
          differenceabs<-sum(absolute)
        	ratio.positive <- (difference.positive)/(differenceabs)
          ratio.negative <- (difference.negative)/(differenceabs)
          pred.value[h1]<-pred.arima[h]
          cat(" The value we predict is",evaluation[1:(h-1)],"\n","Our predicted value is ",pred.arima,"\n")
          cat("    MAPE = ",best,"\n")
          cat("    MIC(+,-)   = ",ratio.positive,",",ratio.negative,"\n","\n")
          cat("    We computed ",h1,"models. We have to compute additional ",(th-h1),"model !!","\n","\n")
 }

 jissoku<-as.vector(jissoku)
 difference<-pred.value-jissoku
 difference.positive <- sum(difference[difference >= 0])
 difference.negative <- -(sum(difference[difference <= 0]))
 absolute<-abs(difference)
 differenceabs<-sum(absolute)
 ratio.positive <- (difference.positive)/(differenceabs)
 ratio.negative <- (difference.negative)/(differenceabs)
 bestmape= sum(absolute/jissoku)*100/length(jissoku)
 cat("####################################################","\n")
 cat("    The total MAPE = ",bestmape,"\n")
 cat("    MIC(+,-)   = ",ratio.positive,",",ratio.negative,"\n")
 cat("    MAPE(+,-) = ",bestmape*ratio.positive,bestmape*ratio.negative,"\n")
 cat("####################################################","\n")
 cat(" JISSOKU Value is", round(as.vector(jissoku),1),"\n","PREDICT Value is")
 return(round(pred.value,1))
 }
