sArimaSAC<-function(y,s="M",jissoku,h=13,span=1,trace=T){
  library(xts)
  library(forecast)
  library(tseries)

  cf<-function(train=train,ap,type){
     count <- 0
     for(x in ap){
         if(abs(x) >= type){count<-count+1}
         else{return(count-1)}
    }}　

  diff_1<-function(y){return(diff(y)[-1])}

  adfk<-function(train){
     adfcount<-1
     y = train
     while(adf.test(y)$p.value >0.05){
     adfcount <- adfcount+1
     y <- diff_1(y)
     }
    return(adfcount)
  }

  if(s=="M"){s<-seq(0,12,6)}
  else if(s=="W"){s<-seq(0,52,26)}

  th<-length(jissoku)+1-span
  traintemp<-y[1:(length(y)-h)]
  evaluationtemp<-rbind(y[(length(y)-(h-1)):length(y)],jissoku)
  pred.value<-array(rep(0,(th)))
  for(h1 in 1:th){
    best<-0
    evaluation<-evaluationtemp[-(1:h1)]
    train<-rbind(traintemp,evaluationtemp[1:h1])
    #print(train)
  #  print(evaluation)
    qres <- try(qtemp<-cf(ap=acf(train,plot=F)$acf,type=0.2)+1,silent=T)
    if(class(qres)=="try-error"){qtemp <- 4}
    p = ifelse((cf(ap=pacf(train,plot=F)$acf,type=0.2)+1)>3,3,cf(ap=pacf(train,plot=F)$acf,type=0.2)+1)
    d = ifelse((adfk(train=train)+1)>3,3,adfk(train=train)+1)
    q = ifelse(qtemp >3,3,cf(ap=acf(train,plot=F)$acf,type=0.2))
    cat("max p = ",p,", max d = ",d,", max q = ",q,"\n")
  for(s1 in s){
    for (i in 0:p){
        for (j in 0:d){
            for(k in 0:q){
                for(P in 0:p){
                    for(D in 0:d){
                        for(Q in 0:q){
                            state= " "
                            res<-try(model<-Arima(train,order=c(i,j,k),seasonal=list(order=c(P,D,Q),period=s1)),silent=T)
                            res2<-try(model2<-Arima(train,order=c(i,j,k),seasonal=list(order=c(P,D,Q),period=s1),include.drift = T),silent=T)
                            if((class(res)=="try-error")&(class(res2)=="try-error")){next}
                            else if(class(res2)=="try-error"){
                              fc.arima <- forecast(model,h=h)
                              pred.arima <- as.vector(fc.arima$mean)
                              if(min(pred.arima)<0){next}
                              difference <- (pred.arima[1:(h-span)])-(evaluation[1:(h-span)])
                              difference.positive <- sum(difference[difference >= 0])
                              difference.negative <- -(sum(difference[difference <= 0]))
                              absolute<-abs(difference)
                              differenceabs<-sum(absolute)
                              ratio.positive <- (difference.positive)/(differenceabs)
                              ratio.negative <- (difference.negative)/(differenceabs)
                              mape=(sum((absolute/evaluation[1:(h-span)])))*100/length(absolute)
                              if(trace==T){
                              cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"]                   MAPE:",mape,"\n")
                              cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"] with drift        skipped","\n")}
                            }
                            else if(class(res)=="try-error"){
                              fc.arima <- forecast(model2,h=h)
                              pred.arima <- as.vector(fc.arima$mean)
                              if(min(pred.arima)<0){next}
                              difference <- (pred.arima[1:(h-span)])-(evaluation[1:(h-span)])
                              difference.positive <- sum(difference[difference >= 0])
                              difference.negative <- -(sum(difference[difference <= 0]))
                              absolute<-abs(difference)
                              differenceabs<-sum(absolute)
                              ratio.positive <- (difference.positive)/(differenceabs)
                              ratio.negative <- (difference.negative)/(differenceabs)
                              mape=(sum((absolute/evaluation[1:(h-span)])))*100/length(absolute)
                              if(trace==T){
                              cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"]                   skipped","\n")
                              cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"] with drift        MAPE:",mape,"\n")}
                            }
                            else{
                            fc.arima <- forecast(model,h=h)
                            pred.arima <- as.vector(fc.arima$mean)
                            if(min(pred.arima)<0){next}
                            difference <- (pred.arima[1:(h-span)])-(evaluation[1:(h-span)])
                            difference.positive <- sum(difference[difference >= 0])
                            difference.negative <- -(sum(difference[difference <= 0]))
                            absolute<-abs(difference)
                            differenceabs<-sum(absolute)
                            ratio.positive <- (difference.positive)/(differenceabs)
                            ratio.negative <- (difference.negative)/(differenceabs)

                            mape=(sum((absolute/evaluation[1:(h-span)])))*100/length(absolute)

                            fc.arima2 <- forecast(model2,h=h)
                            pred.arima2 <- as.vector(fc.arima2$mean)
                            if(min(pred.arima2)<0){next}
                            difference2 <- (pred.arima2[1:(h-span)])-(evaluation[1:(h-span)])
                            difference.positive2 <- sum(difference2[difference2 >= 0])
                            difference.negative2 <- -(sum(difference2[difference2 <= 0]))
                            absolute2<-abs(difference2)
                            differenceabs2<-sum(absolute2)
                            ratio.positive2 <- (difference.positive2)/(differenceabs2)
                            ratio.negative2 <- (difference.negative2)/(differenceabs2)

                            mape2=(sum((absolute2/evaluation[1:(h-span)])))*100/length(absolute2)
                            if(trace==T){
                            cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"]                   MAPE:",mape,"\n")
                            cat("(",i,j,k,")","(",P,D,Q,")","[",s1,"] with drift        MAPE:",mape2,"\n")}
                            if(mape2<mape){
                              state="with drift"
                              mape<-mape2
                              pred.arima2<-pred.arima
                              ratio.positive<-ratio.positive2
                              ratio.negative<-ratio.negative2

                          }
}

                            if(best==0){
                                  best<-mape
                                  besti<-i
                                  bestj<-j
                                  bestk<-k
                                  bestP<-P
                                  bestD<-D
                                  bestQ<-Q
                                  bests<-s1
                                  bestpred<-pred.arima
                                  bestratio.p<-ratio.positive
                                  bestratio.n<-ratio.negative
                                  beststate<-state
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
                                  bestpred<-pred.arima
                                  bestratio.p<-ratio.positive
                                  bestratio.n<-ratio.negative
                                  beststate<-state
                                    }
                                  }}}}}}}
          cat("\n","The best model is  (",besti,",",bestj,",",bestk,")(",bestP,",",bestD,",",bestQ,")[",bests,"]",beststate,"\n")
          pred.value[h1]<-bestpred[h]
          cat(" The value we predict is",evaluation[1:(h-span)],"\n","Our predicted value is ",bestpred,"\n")
          cat("    MAPE = ",best,"\n")
          cat("    MIC(+,-)   = ",bestratio.p,",",bestratio.n,"\n","\n")
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
 RMSE<-sqrt(sum(difference^2)/length(difference))
 cat("####################################################","\n")
 cat("    MAPE = ",bestmape,"\n")
 cat("    RMSE = ",RMSE,"\n")
 cat("    MIC(+,-)   = ",ratio.positive,",",ratio.negative,"\n")
 cat("    MAPE(+,-) = ",bestmape*ratio.positive,bestmape*ratio.negative,"\n")
 cat("####################################################","\n")
 cat(" JISSOKU Value is", round(as.vector(jissoku),1),"\n","PREDICT Value is")
 return(pred.value)
 }
