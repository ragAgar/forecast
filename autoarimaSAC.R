autoSAC<-function(y,s="W",jissoku,span=2,trace=F,standard=T){
  library(xts)
  library(forecast)
  library(tseries)


  cf<-function(train=train,ap,type){
     count <- 0
     for(x in ap){
         if(abs(x) >= type){count<-count+1}
         else{
           return(count-1)
         }}}
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

  if(standard == T){
    MEAN<-mean(y);SD<-sd(y)
    y <- (y-MEAN)/SD
  }
  if(s=="M"){s<-c(0,6,12)}
  else if(s=="W"){s<-c(0,26,52)}
  th<-length(jissoku)+(1-span)
  traintemp<-y[1:(length(y)-1)]
  evaluationtemp<-rbind(y[length(y)],jissoku)
  pred.value<-array(rep(0,(th)))

  for(h1 in 1:th){
    count=0
    count2=0
    count3=0
    evaluation<-evaluationtemp[-(1:h1)]
    train<-rbind(traintemp,evaluationtemp[1:h1])
    pres <- try(ptemp<-cf(ap=pacf(train,plot=F)$acf,type=0.2)+1,silent=T)
    qres <- try(qtemp<-cf(ap=acf(train,plot=F)$acf,type=0.2)+1,silent=T)
    dres <- try(dtemp<-(adfk(train=train)-1),silent=T)

    if(class(qres)=="try-error"){qtemp <- 4}
    if(class(pres)=="try-error"){ptemp <- 4}
    if(class(dres)=="try-error"){dtemp <- 4}
    p = ifelse(ptemp>3,3,cf(ap=pacf(train,plot=F)$acf,type=0.2)+1)
    d = ifelse(dtemp>3,3,adfk(train=train)+1)
    q = ifelse(qtemp>3,3,cf(ap=acf(train,plot=F)$acf,type=0.2))

    cat("max p = ",p,", max d = ",d,", max q = ",q,"\n")

      for(k in 0:d){

               model<-auto.arima(x= train,max.p = p,max.P = p,max.q = q,max.Q=q,max.d=d,max.D=d,d=k,
                                  start.p=0,start.q=0,start.P=0,start.Q=0,stepwise=F,ic="aic",approximation=F,
                                  test="adf",trace=trace,seasonal=T,allowmean=F,allowdrift=T)
                                  if(count==0){bestmodel<-model;count=1}
                                  else if(bestmodel$aic > model$aic){bestmodel<-model}
          for(j in 0:d){
               model2<-auto.arima(x= ts(train,frequency=s[2]),max.p = p,max.P = p,max.q = q,max.Q=q,max.d=d,max.D=d,
                                  start.p=0,start.q=0,start.P=0,start.Q=0,stepwise=F,ic="aic",approximation=F,d=k,D=j,
                                  test="adf",trace=trace,seasonal=T,allowmean=F,allowdrift=T)
                                  if(count2==0){bestmodel2<-model2;count=1}
                                  else if(bestmodel2$aic > model2$aic){bestmodel2<-model2}
               model3<-auto.arima(x= ts(train,frequency=s[3]),max.p = p,max.P = p,max.q = q,max.Q=q,max.d=d,max.D=d,
                                  start.p=0,start.q=0,start.P=0,start.Q=0,stepwise=F,ic="aic",approximation=F,d=k,D=j,
                                  test="adf",trace=trace,seasonal=T,allowmean=F,allowdrift=T)
                                  if(count3==0){bestmodel3<-model3;count=1}
                                  else if(bestmodel3$aic > model3$aic){bestmodel3<-model3}
      #                            cat(bestmodel$aic,bestmodel2$aic,bestmodel3$aic)
               if(min(bestmodel$aic,bestmodel2$aic,bestmodel3$aic)==bestmodel$aic){bestmodel<-bestmodel}
               else if(min(bestmodel$aic,bestmodel2$aic,bestmodel3$aic)==bestmodel2$aic){bestmodel<-bestmodel2}
               else if(min(bestmodel$aic,bestmodel2$aic,bestmodel3$aic)==bestmodel3$aic){bestmodel<-bestmodel3}
}}
          fc.arima <- forecast(bestmodel,h=span)
          pred.arima <- as.vector(fc.arima$mean)
        #  if(min(pred.arima)<0){next}
          difference <- (pred.arima[1:span])-(evaluation[1:span])
          mape=(sum(abs(difference)/evaluation[1:span]))*100/length(difference)
          pred.value[h1]<-pred.arima[span]
          cat(" The best model is SARIMA(",bestmodel$arma[1],",",bestmodel$arma[6],",",bestmodel$arma[2],")(",bestmodel$arma[3],",",bestmodel$arma[7],",",bestmodel$arma[4],")[",bestmodel$arma[5],"]","\n")
        #  print(summary(bestmodel))
          cat(" The value we predict is",evaluation[1:span],"\n","Our predicted value is ",pred.arima,"\n")
          cat("    MAPE = ",mape,"\n")
          cat("    We computed ",h1,"models. We have to compute additional ",(th-h1),"model !!","\n","\n")
 }
 if(standard==T){
   pred.value=SD*pred.value+MEAN
 }
 jissoku<-as.vector(jissoku)
 jissoku<-jissoku[span:length(jissoku)]
 difference<-pred.value-jissoku
 difference.positive <- sum(difference[difference >= 0])
 difference.negative <- -(sum(difference[difference <= 0]))
 absolute<-abs(difference)
 differenceabs<-sum(absolute)
 ratio.positive <- (difference.positive)/(differenceabs)
 ratio.negative <- (difference.negative)/(differenceabs)
 bestmape= sum(absolute/jissoku)*100/length(jissoku)
 RMSE<-sqrt(sum(difference^2)/length(difference))
 cat("------------------------------------------------------","\n")
 cat("    MAPE = ",bestmape,"\n")
 cat("    COR = ",cor(jissoku,pred.value),"\n")
 cat("    MIC(+,-)   = ",ratio.positive,",",ratio.negative,"\n")
 cat("    MAPE(+,-) = ",bestmape*ratio.positive,bestmape*ratio.negative,"\n")
 cat("------------------------------------------------------","\n")
 cat(" JISSOKU Value is", round(as.vector(jissoku),1),"\n","PREDICT Value is")
 return(pred.value)
 }
