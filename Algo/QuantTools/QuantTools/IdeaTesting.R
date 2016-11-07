library( QuantTools )
# load ticks data set
data( ticks )
ticks


Rcpp::sourceCpp( 'strategy/VolUp.cpp' )


timeframe = 60 # seconds
n = 30
period_long = 100
period_short = 20
latency = 0.1 # 100 milliseconds
# see how fast back testing done on over 2 millin ticks
system.time( { x = VolUp( ticks, n, period_long, period_short, timeframe, latency ) } )
x$summary


x = VolUp( ticks[ time %bw% '2016' ], n, period_long, period_short, timeframe, latency )
x


# leave only closed trades and executed orders
x$trades = x$trades[ state == 'closed' ]
x$orders = x$orders[ state == 'executed' ]

interval = '2016'

layout( matrix( 1:2, ncol = 1 ), height = c( 2, 1 ) )

par( mar = c( 0, 4, 2, 4 ), family = 'sans' )
par( xaxt = 'n' )
plot_ts( x$indicators[ time %bw% interval ], type = 'candle' )
plot_ts( x$indicators[ ,.( time, sma_long, sma_short ) ],
         col = c( 'goldenrod', 'darkmagenta' ), legend = 'topleft', add = T )
plot_ts( x$orders[ side == 'buy', .( time_processed, price_exec ) ],
         type = 'p', pch = 24, col = 'blue', legend = 'n', last_values = F, add = T )
plot_ts( x$orders[ side == 'sell', .( time_processed, price_exec ) ],
         type = 'p', pch = 25, col = 'red' , legend = 'n', last_values = F, add = T )

calc_dd = function( pnl ) { x = pnl; x[ x < 0 ] = 0; pnl - cummax( x ) }
pnl = x$trades[ time_exit %bw% interval, .( time_exit, pnl = cumsum( pnl_rel ), dd = calc_dd( cumsum( pnl_rel ) ) ) ]

par( xaxt = 's', mar = c( 4, 4, 0, 4 ) )
plot_ts( x$indicators[ time %bw% interval ], type = 'n', ylim = pnl[, range( 0, pnl, dd ) ] )
plot_ts( pnl, lwd = 1, type = 's', legend = 'bottomleft', add = T )



# create parameters combinations
parameters = CJ(
  latency      = 0:1 * 1,
  n = seq(30,60,10),
  timeframe    = c( 30, 60 ) * 60,
  period_long  = seq(50,100,5),
  period_short = seq(5,50,5)
)
# preview parameters
parameters


system.time({
  tests = parameters[, VolUp( ticks, n, period_long, period_short, timeframe, latency, fast = T ), 
                     by = .( test_id = 1:nrow( parameters ) ) ]
})


# preview tests result
tests

dev.new(width=6, height=4)
multi_heatmap( cbind( parameters, tests ), names( parameters ), 'pnl' ) 
