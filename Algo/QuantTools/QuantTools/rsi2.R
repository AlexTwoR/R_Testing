library( QuantTools )

#Load Ticks
ticks_raw<-read.csv(file='data/US1.IBM_160101_161031.csv', sep=";")

ticks<-as.data.table(ticks_raw)
names(ticks)<-c('time1','time2','price', 'volume', 'id')

head(ticks)

#pars time
ticks$time1 <- as.Date(ticks$time1, "%d/%m/%y")
ticks$time  <- as.POSIXct(paste(ticks$time1, ticks$time2))


ticks$time1 = NULL
ticks$time2 = NULL

class(ticks)
typeof(ticks)
str(ticks)


Rcpp::sourceCpp('strategy/rsi.cpp')


timeframe = 1 # seconds
n = 14
up = 70.0
down = 30.0
latency = 0.1 # 100 milliseconds
# see how fast back testing done on over 2 millin ticks
system.time( { x = rsi(ticks , n, up, down, timeframe, latency) } )


x = rsi( ticks[ time %bw% '2016-09' ], n, up, down, timeframe, latency )
plot(x$indicators$close, type='l')
plot(x$indicators$rsi, type='l')
x$summary

# leave only closed trades and executed orders
x$trades = x$trades[ state == 'closed' ]
x$orders = x$orders[ state == 'executed' ]

interval = '2016-09'

layout( matrix( 1:2, ncol = 1 ), height = c( 2, 1 ) )

par( mar = c( 0, 4, 2, 4 ), family = 'sans' )
par( xaxt = 'n' )
plot_ts( x$indicators[ time %bw% interval ], type = 'candle' )


plot_ts( x$orders[ side == 'buy', .( time_processed, price_exec ) ],
         type = 'p', pch = 24, col = 'darkseagreen', legend = 'n', last_values = F, add = T )
plot_ts( x$orders[ side == 'sell', .( time_processed, price_exec ) ],
         type = 'p', pch = 25, col = 'firebrick' , legend = 'n', last_values = F, add = T )

plot_ts( x$indicators[ ,.( time, rsi ) ], col = 'firebrick', mar = c( 5.1, 4.1, 0, 2.1 ),  add = F )

rsi(x$indicators$close,14)


calc_dd = function( pnl ) { x = pnl; x[ x < 0 ] = 0; pnl - cummax( x ) }
pnl = x$trades[ time_exit %bw% interval, .( time_exit, pnl = cumsum( pnl_rel ), dd = calc_dd( cumsum( pnl_rel ) ) ) ]

par( xaxt = 's', mar = c( 4, 4, 0, 4 ) )
plot_ts( x$indicators[ time %bw% interval ], type = 'n', ylim = pnl[, range( 0, pnl, dd ) ] )
plot_ts( pnl, lwd = 1, type = 's', legend = 'bottomleft', add = T )


# create parameters combinations
parameters = CJ(
  latency      = 0:1 * 1,
  timeframe    = c(30,60,180),
  n = seq(20,60,10),
  up = seq(75,95,5),
  down = seq(10,45,5)
  
)

parameters

system.time({
  tests = parameters[, rsi( ticks, n, up, down, timeframe, latency, fast = T ),
                     by = .( test_id = (1:nrow( parameters )) )]
})

# preview tests result
tests

dev.new(width=5, height=4)
multi_heatmap( cbind( parameters, tests ), names( parameters ), 'n') 

