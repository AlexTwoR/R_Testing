rm(list = ls(all = TRUE))

#http://etfprophet.com/days-since-200-day-highs/

#выгрузка
require(quantmod)
getSymbols('^GSPC',from='1900-01-01')

#считаем дни
daysSinceHigh <- function(x, n){
  apply(embed(x, n), 1, which.max)-1
}

#embed строит ряд со значениями назад

myStrat <- function(x, nHold=100, nHigh=200) {
  position <- ifelse(daysSinceHigh(x, nHigh)<=nHold,1,-1)
  c(rep(0,nHigh-1),position)
}

xx<-sample(c(1:20))
xx[5:20 ] /  (rollapply(xx, 5, sd))



myStrat <- function(x, nHold=100, nHigh=200) {
  #position <- ifelse(daysSinceHigh(x, nHigh)<=nHold,1,-1)
  position<-rollapply(x, 30, mean)/rollapply(x, 30, sd)
  
  #c(rep(0,30-1),position)
  position
}

myStock <- Cl(GSPC)
myPosition <- myStrat(myStock,200,300)
bmkReturns <- dailyReturn(myStock, type = "arithmetic")
myReturns <- bmkReturns*Lag(myPosition,1) #позиции на доходности
length(myPosition)
myReturns[1] <- 0
charts.PerformanceSummary(cbind(bmkReturns,myReturns))
x<-myStock
tail(rollapply(x, 30, mean)/rollapply(x, 30, sd))


names(bmkReturns) <- 'SP500'
names(myReturns) <- 'Me'


require(PerformanceAnalytics)
charts.PerformanceSummary(cbind(bmkReturns,myReturns))


row.names(myReturns) <- levels(myReturns$date)[myReturns$date]
myReturns <- myReturns[, 'Me', drop=FALSE]
