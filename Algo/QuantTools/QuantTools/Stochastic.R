library( QuantTools )


ticks2frame <- function(ticks_raw){
  
  ticks<-as.data.table(ticks_raw)
  names(ticks)<-c('time1','time2','price', 'volume', 'id')
  
  #head(ticks)
  
  #pars time
  ticks$time1 <- as.Date(ticks$time1, "%d/%m/%y")
  ticks$time  <- as.POSIXct(paste(ticks$time1, ticks$time2))
  
  #Delete old cols
  ticks$time1 = NULL
  ticks$time2 = NULL
  
  #class(ticks)
  #typeof(ticks)
  #str(ticks)
  
  ticks
  
}

#Load Ticks
ticks_raw<-read.csv(file='data/US1.IBM_160101_161031.csv', sep=";")

ticks <- ticks2frame(ticks_raw)
head(ticks)

data(ticks)

Rcpp::sourceCpp( 'strategy/stochastic.cpp' )

timeframe = 1800 # seconds
n = 55
fast = 5
slow = 14
latency = 0.1 # 100 milliseconds
# see how fast back testing done on over 2 millin ticks
system.time( { x = stochastic( ticks, n, fast, slow, timeframe, latency ) } )
x$summary
x$indicators

x = stochastic( ticks[ time %bw% '2016' ], n, fast, slow, timeframe, latency )

# leave only closed trades and executed orders
x$trades = x$trades[ state == 'closed' ]
x$orders = x$orders[ state == 'executed' ]

interval = '2016'

layout( matrix( 1:2, ncol = 1 ), height = c( 2, 1 ) )

par( mar = c( 0, 4, 2, 4 ), family = 'sans' )
par( xaxt = 'n' )
plot_ts( x$indicators[ time %bw% interval ], type = 'candle' )

plot_ts( x$orders[ side == 'buy', .( time_processed, price_exec ) ],
         type = 'p', pch = 24, col = 'blue', legend = 'n', last_values = F, add = T )
plot_ts( x$orders[ side == 'sell', .( time_processed, price_exec ) ],
         type = 'p', pch = 25, col = 'red' , legend = 'n', last_values = F, add = T )

plot_ts( x$indicators[ ,.( time, stoch_kFast, stoch_dSlow ) ],
         col = c( 'goldenrod', 'darkmagenta' ), legend = 'topleft', add = F )

calc_dd = function( pnl ) { x = pnl; x[ x < 0 ] = 0; pnl - cummax( x ) }
pnl = x$trades[ time_exit %bw% interval, .( time_exit, pnl = cumsum( pnl_rel ), dd = calc_dd( cumsum( pnl_rel ) ) ) ]

par( xaxt = 's', mar = c( 4, 4, 0, 4 ) )
plot_ts( x$indicators[ time %bw% interval ], type = 'n', ylim = pnl[, range( 0, pnl, dd ) ] )
plot_ts( pnl, lwd = 1, type = 's', legend = 'bottomleft', add = T )



# create parameters combinations
parameters = CJ(
  latency      = 0:1 * 1,
  timeframe    = c(1, 10, 30 ) * 60,
  n = seq(40,100,10),
  fast  = 3:5,
  slow = 14:16,
  up = seq(70,80,10),
  down = seq(30,40,10)
)
# preview parameters
parameters


  system.time({
    tests = parameters[, stochastic( ticks, n, fast, slow, timeframe, latency, fast = T, up, down ), 
                       by = .( test_id = 1:nrow( parameters ) ) ]
  })


dev.new(width=6, height=4)
multi_heatmap( cbind( parameters, tests ), names( parameters ), 'n' ) 