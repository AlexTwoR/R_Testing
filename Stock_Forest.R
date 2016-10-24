library(rusquant)
library(quantmod)

getSymbols('GAZP', from='2010-01-01', src='Finam')

#��� ������������� �����������
set.seed(1)

#Plots
plot(GAZP)


#define returns
ret<-Cl(GAZP)/Lag(Cl(GAZP))-1

#up down 
mov <- ret
mov[mov>=0]<-"Up"
mov[mov<0]<-"Down"
names(mov)="Move"
mov
data <- lag(GAZP)

#define params
data.port<-cbind( 
  lag(ret)*100, 
  Vo(data), 
  RSI(Cl(data)),  
  MACD(Cl(data)), 
  volatility(data), 
  as.numeric( factor(weekdays(index(ret))) )
)

data.or<-data.port
#data.port <- cbind(mov, data.port)
data.port <- data.port[complete.cases(data.or),]
mov<-mov[complete.cases(data.or),]
length(data.port[,1])
names(data.port)[6]<-"Vol"
names(data.port)[7]<-"DayWeek"

head(data.or)

tail(data.port)
tail(mov)

length(data.port[,1])
length(mov)
#test and train

N<-250
T<-length(data.port[,1])
ret_tr <- data.port[1:(T-N),]
ret_ts <- data.port[(T-N+1):T,]
mov.tr <- mov[1:(T-N),]
mov.ts <- mov[(T-N+1):T,]

library(tree)
sum(is.na(ret_tr))

head(ret_tr)
tree.fit<-tree(mov.tr ~ ., data=ret_tr)
?tree
plot(tree.fit)
text(tree.fit)

predictions <- predict(tree.fit, ret_ts)
predictions

#random forest
library(randomForest)
head(ret_tr)
rf <- randomForest(mov.tr ~ ., ret_tr)

predictions <- predict(rf, ret_ts[,-1])
predictions==ret_ts[,1]


ret_ts[-20,1]
GAZP[-20,1]



library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)


summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")

Carseats
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)


??forecast


