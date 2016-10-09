
# Frame rules to trade EUR/USD using Support Vector Machine (SVM) algorithm

library(quantmod)
library(TTR)
library(e1071)
library(ggplot2)

# Read the EUR/USD data and use indicators to build the model ###################################################################################
# Using MACD histogram and Parabolic SAR as indicators

data = read.csv("EURUSD_hour.csv")
data = data[,c(2:7)]
colnames(data) = c("Date","Time","Open","Low","High","Close")

macd_data = MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, maType="EMA", percent=FALSE)
sar       = SAR(data[,c("High","Low")], accel = c(0.02, 0.2))
trend     = data$Close - sar

price = data$Close-data$Open
class = ifelse(price > 0,"UP","DOWN")

# Merging the indicators and creating Training and Test data set ################################################################################

macd_data = data.frame(macd_data)
histogram = macd_data$macd - macd_data$signal
histogram = c(NA,head(histogram,-1))  # creating a lag to avoid look-ahead bias

trend = c(NA,head(trend,-1)) # creating a lag to avoid look-ahead bias

model_data = data.frame(class,trend,histogram)
model_data = model_data[-c(1:34),]

breakpoint    = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint, ]
test_data     = model_data[(breakpoint+1):nrow(model_data), ]

# Use the SVM algorithm to find patterns and then make predictions ##########################################################################

SVM = svm(class~trend+histogram,data=training_data, kernel="radial",cost=1,gamma=1/2)
trainingpredictions = predict(SVM,training_data,type="class")
trainingdata = data.frame(training_data,trainingpredictions)
accuracy=(sum(trainingdata$trainingpredictions == trainingdata$class)/nrow(trainingdata))*100
print(accuracy)

# Plot the pattern using the ggplot package ########################################################################################################

ggplot(trainingdata,aes(x=histogram,y=trend)) +
  stat_density2d(geom="contour",aes(color=trainingpredictions)) +
  labs(title="SVM Predictions", x="MACD", y="Price - SAR",color="Training Predictions")

# Define rules for short/long trades and test the accuracy of predictions on test data ################################################################################

# rule for short trades
trend_shortrule = (test_data$trend > -0.0025 & test_data$trend < 0.0100)
histogram_shortrule = (test_data$histogram > -0.0010 & test_data$histogram < 0.0010)
ShortRange = which(trend_shortrule & histogram_shortrule)
ShortTrades = test_data[c(ShortRange),]
ShortAccuracy = ((length(which(ShortTrades[,1]=="DOWN")))/nrow(ShortTrades))*100
print(ShortAccuracy)

# rule for long trades
trend_longrule = (test_data$trend > -0.0150 & test_data$trend < -0.0050)
histogram_longrule = test_data$histogram > -0.0005 
LongRange = which(trend_longrule & histogram_longrule)
LongTrades = test_data[c(LongRange), ]
LongAccurancy = ((length(which(LongTrades[,1]=="UP")))/nrow(LongTrades))*100
print(LongAccurancy)






