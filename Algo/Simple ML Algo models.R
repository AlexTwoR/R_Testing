getFeature = function(assets){
  
  #define returns
  ret<-Cl(asset)/Lag(Cl(asset))-1
  
  #------ up down 
  mov <- ret
  
  mov[mov>=0]<-"Up"
  mov[mov<0]<-"Down"
  names(mov)="Move"
  
  #set Lag
  data <- lag(asset)
  
  #define params
  data.port<-cbind( 
    lag(ret)*100, 
    Vo(data), 
    RSI(Cl(data)),
    volatility(data), 
    as.numeric( factor(weekdays(index(ret))) )
  )
  
  head(data.port)
  
  #save copy of port
  data.or<-data.port
  
  data.port <- data.port[complete.cases(data.or),]
  mov<-mov[complete.cases(data.or),]
  
  length(data.port[,1])
  length(mov)
  
  names(data.port)[4]<-"Vol"
  names(data.port)[5]<-"DayWeek"
  
  list(mov=mov, data.port=data.port)
}



#=========== Get Data =========== 

library(quantmod)
set.seed(1)

getSymbols("AAPL", from="2011-01-01", to="2016-01-10")

asset<-AAPL

#define returns
ret<-Cl(asset)/Lag(Cl(asset))-1

port<-getFeature(asset)

length(port$mov)==length(port$data.port[,1])


dat=data.frame(port$data.port,y=as.factor(port$mov))
head(dat)

library(corrplot)
corrplot(cor(dat[,1:(length(dat)-1)]), method="square", order="hclust")


ixTrain <- sample(1:nrow(dat),round(0.75*nrow(dat)))
train <- dat[ixTrain,]
test <- dat[-ixTrain,]

#=========== SVM =========== 
library(e1071)

svmfit=svm(y~., data=dat[ixTrain,], kernel="polynomial",  gamma=0.5, cost=10)
summary(svmfit)

pred=predict(svmfit,newdata=dat[-ixTrain,])

table(pred,dat[-ixTrain,6])

length(pred[pred==dat[-ixTrain,6]])/length(pred)


set.seed(1)
tune.out=tune(svm, y~., data=dat[ixTrain,], kernel="polynomial", ranges=list(cost=c(0.1,1,10, 50),gamma=c(0.1,0.5)))
summary(tune.out)



pred=predict(tune.out$best.model,newdata=dat[-ixTrain,])
table(pred,dat[-ixTrain,6])

length(pred[pred==dat[-ixTrain,6]])/length(pred)
