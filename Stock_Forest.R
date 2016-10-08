library(rusquant)
library(quantmod)

getSymbols('GAZP', from='2005-05-01', src='Finam')

#для повторяемости результатов
set.seed(1)

#Plots
plot(GAZP)
chartSeries(GAZP, subset='last 12 months')
addBBands()

#Volume changing
label <- (Vo(GAZP) - lag(Vo(GAZP)))/lag(Vo(GAZP))

names(label) <- c('label')
data <- lag(GAZP)

data <-cbind(lag(label), lag(label, k=2), lag(label, k=3), lag(label, k=4), lag(label, k=5), RSI(Cl(data)), MACD(Cl(data)), volatility(data), factor(weekdays(index(label))))
names(data) <- c('Volume1', 'Volume2', 'Volume3', 'Volume4', 'Volume5', 'RSI', 'macd', 'signal', 'volatility', 'weekdays')
data <- cbind(label, data)
data <- data[complete.cases(data),]

# 80% данных используем для тренировки модели, 20% для тестирования
split <- runif(dim(data)[1]) > 0.2
train <- data[split,]
test <- data[!split,]


#random forest
library(randomForest)
rf <- randomForest(label ~ ., train)
predictions <- predict(rf, test)
print(sqrt(sum((as.vector(predictions - test$label))^2))/length(predictions))

test.n<-as.numeric(test[,1])
plot(test.n, type="l")
lines(as.numeric(predictions)+1, col="red")
