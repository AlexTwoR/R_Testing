library(Quandl)	

#CUR/RUB
#7d26x_bmdjAWyobrqCmD

Quandl.api_key("7d26x_bmdjAWyobrqCmD")

rubp = Quandl("CUR/RUB", start_date="2015-01-01")
library(Quandl)#CLSH/EURSEK_SPT_VOL
oil.ts <- Quandl("CLSH/EURSEK_SPT_VOL", trim_start="2016-01-01", trim_end="2016-02-01", type="zoo")

plot(rubp, type="l")

head(rubp)

price<-rubp$RATE


mean(price)
var(price)
sd(price)
c(mean(price)+sd(price), mean(price)-sd(price))
boxplot(price)

ret<-diff(price)/price[-1]
plot(ret,type="l")

#hist(ret,25)

shapiro.test(ret)
ks.test(ret, "pnorm", mean = mean(ret), sd = sd(ret))


hist(ret, breaks = 20, freq = FALSE)
points <- seq(min(ret), max(ret), length.out = 100)
lines(points, dnorm(points, mean = mean(ret), sd = sd(ret)), col=2)

w.st <- (ret - mean(ret))/sd(ret)

qqnorm(w.st) # Рисуем график
qqline(w.st) # Добавляем линию квартилей

library(ghyp)

ghyp_dist <- fit.ghypuv(ret, silent = TRUE)
hist(ghyp_dist)
qqghyp(ghyp_dist)

aic <- stepAIC.ghyp(ret, dist=c( "hyp", "t", "gauss"), silent=TRUE)

aic$best


qghyp(0.05, object = aic$best.model)

N <- 10
test <- ret[(N+1):length(ret)]
VaR <- rep(0, length(test))
for (i in (N+1):length(ret)){
  train <- ret[(i-N):(i-1)]
  model <- stepAIC.ghyp(train, dist=c("hyp", "t", "gauss"), silent=T)$best.model
  VaR[i-N] <- qghyp(0.05, object = model)
}
plot(test, type="l", main = "Кривая VaR для нормальных доходностей")
lines(VaR, col="red")

VaR
L






library(qmao)

getQuote("SPY", src="google")


