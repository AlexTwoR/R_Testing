install.packages("devtools") 
require(devtools) 
install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org
                 <http://r-forge.r-project.org/>") 
install.packages("blotter", repos="http://R-Forge.R-project.org
                 <http://r-forge.r-project.org/>") 
install.packages("quantstrat", repos="http://R-Forge.R-project.org
                 <http://r-forge.r-project.org/>") 
install.packages("foreach") 
install_github("IlyaKipnis/IKTrading") 

require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)


initDate="1990-01-01"
from="2003-01-01"
to="2012-12-31"
options(width=70)


#=================================================
options("getSymbols.warning4.0"=FALSE)

currency('USD')
Sys.setenv(TZ="UTC")

symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)

#SPDR ETFs first, iShares ETFs afterwards
if(!"XLB" %in% ls()) { 
  suppressMessages(getSymbols(symbols, from=from, to=to, src="yahoo", adjust=TRUE))  
}

stock(symbols, currency="USD", multiplier=1)


#=================================================




#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "DollarVsATRos"
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)
