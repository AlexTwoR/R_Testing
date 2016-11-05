data_raw=read.table(file = 'data/Data1.asc', header = TRUE)
data<-data.frame(data_raw)
head(data)


data$Date<-as.POSIXct(paste(data$Дата, data$Время0), format="%d-%m %H:%M:%S")

data$Время=NULL
data$Время0=NULL
data$Дата=NULL
data$Номер=NULL

data<-data.frame(Date=data$Date, Value=data$Значение)

head(data)

data_n<-data[1:500,]

plot(data_n, type='p')


mean(data_n$Value)



#------- Пример ---------------
library(dlm)
library(quantmod)

getSymbols("^GSPC", from="2010-09-01")
y <- as.ts(Cl(GSPC))
x <- dlmFilter(y, dlmModPoly(1, dV=10, dW=0.1))

plot(y, type='l')
lines(x$f[-1], col = "red")
#Добавим EMA для сравнения
lines(EMA(y), col = "green")

#----------------

y <- as.ts(data_n$Value)
x <- dlmFilter(y, dlmModPoly(1, dV=10, dW=0.1))

plot(y, type='l')
lines(x$f[-1], col = "red")

