library(rusquant)
library(quantmod)

getSymbols('GAZP', from='2010-01-02')
getSymbols("TSLA", from="2014-01-01", to="2016-09-01")

head(TSLA)
asset<-TSLA

#для повторяемости результатов
set.seed(1)

#Plots
plot(asset)


#define returns
ret<-Cl(asset)/Lag(Cl(asset))-1

#up down 
mov <- ret
mov[mov>=0]<-"Up"
mov[mov<0]<-"Down"
names(mov)="Move"
head(mov)

#define params
data <- lag(TSLA)

port <-cbind( 
  lag(ret)*100, 
  Vo(data),
  volatility(data)
 
)

#del NA
head(port)
data.or<-port
port <- port[complete.cases(data.or),]
mov<-mov[complete.cases(data.or),]

#rename
port<-data.frame(port,mov)
names(port)[3]<-"Vol"

head(port)


N<-50
T<-length(port[,1])
ret.tr <- port[1:N,]
ret.ts <- port[(N+1):T,]

length(ret.ts[,1])

#forest for one sample
port.fit <- tree(Move~.,port[1:55,])

summary(port.fit)

plot(port.fit)
text(port.fit,pretty=0)

port.pred=predict(port.fit,ret.ts[1,],type="class")
as.numeric(port.pred)
typeof(port.pred)


N<-55
T
anw<-c()

#smooth
for (i in 1:(T-N-1)) {
  port.fit <- tree(Move~., port[i:(N+i-1),])
  anw[i]<-predict(port.fit,port[N+i,],type="class")
}
head(anw)

anw=ifelse(anw==1,"Down","Up")
length(anw)
length(port[(N+1):(T-1),1])
port[(N+1):(T-1),1]
table(anw,port[(N+1):(T-1),4])

#accorency
sum(anw==port[(N+1):(T-1),4])/(T-N)

#smooth
for (i in 1:(T-N-1)) {
  port.fit <- tree(Move~., port[i:(N+i-1),])
  anw[i]<-predict(port.fit,port[N+i,],type="class")
}



#----- random forest -----
library(randomForest)

port.rf <- randomForest(Move~.,port[1:55,])
summary(port.rf)

plot(port.rf)
text(port.rf,pretty=0)

port.pred=predict(port.fit,port[56,],type="class")
port.pred
port[56,4]
importance(port.rf)



for (i in 1:(T-N-1)) {
  port.rf <- randomForest(Move~., port[i:(N+i-1),])
  anw[i]<-predict(port.fit,port[N+i,],type="class")
}

anw=ifelse(anw==1,"Down","Up")

#accorency
table(anw,port[(N+1):(T-1),4])
sum(anw==port[(N+1):(T-1),4])/(T-N)
