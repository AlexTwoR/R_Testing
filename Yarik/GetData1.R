data_raw=read.table(file = 'data/Data1.asc', header = TRUE)
data<-data.frame(data_raw)
head(data)

str(data)

data$Date<-as.POSIXct(paste(data$Дата, data$Время0), format="%d-%m %H:%M:%S")

data$Время=NULL
data$Время0=NULL
data$Дата=NULL
data$Номер=NULL

data<-data.frame(Date=data$Date, Value=data$Значение)

head(data)

data_n<-data[1:50,]

plot(data_n, type='l')


y <- as.ts(data_n$Value)
x <- dlmFilter(y, dlmModPoly(1, dV=10, dW=1))

plot(y, type='p')
lines(x$f[-1], col = 2)
lines(EMA(y), col = "green")



#------- Пример -------
library(dlm)
library(quantmod)

getSymbols("^GSPC", from="2010-09-01")
y <- as.ts(Cl(GSPC))
x <- dlmFilter(y, dlmModPoly(1, dV=1, dW=5))

plot(y, type='l')
lines(x$f[-1], col = "red")
#Добавим EMA для сравнения
lines(EMA(y), col = "green")
#----------------






