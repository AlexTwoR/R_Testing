library(quantmod)


getSymbols("AAPL")
chartSeries(AAPL, subset='last 12 months')

addBBands()
?addBBands
