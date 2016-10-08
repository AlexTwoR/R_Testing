library(rusquant)
library(quantmod)

getSymbols('GAZP', from='2010-01-01', src='Finam')

#для повторяемости результатов
set.seed(1)

#Plots
plot(GAZP)
chartSeries(GAZP, subset='last 12 months')


#define returns
ret<-Cl(GAZP)/Lag(Cl(GAZP))-1

#up down 
mov <- ret
mov[mov<0]<-0
mov[mov>0]<-1
names(mov)="Move"

data <- lag(GAZP)

#define params
data.port<-cbind( 
  lag(ret), 
  Vo(data), 
  RSI(Cl(data)),  
  MACD(Cl(data)), 
  volatility(data), 
  factor(weekdays(index(ret)))
)

data <- cbind(mov, data)
data <- data[complete.cases(data),]


#test and train
N<-250
T<-length(mov)
ret_tr <- mov[1:(T-N)]
ret_ts <- mov[(T-N+1):T]


