// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List bbands( 
    DataFrame ticks, 
    int n,           // bbands period
    double k,        // bbands number of standard deviations
    int timeFrame,   // candle priod in seconds
    double latency,   // round trip latency in seconds
    bool fast = false // return summary only
) {
  
  // define strategy states
  enum class ProcessingState{ LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;
  
  // initialize indicators
  BBands bbands( n, k );
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  bt.SetCost( cost );
  
  // define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    
    // add values to indicators
    bbands.Add( candle.close );
    
  };
  
  // define what to do when new tick arrived
  bt.onTick = [&]( Tick tick ) {
    
    // if moving averages not formed yet do nothing
    if( not bbands.IsFormed() ) return;
    
    // if strategy has no position
    if( state == ProcessingState::FLAT) {
      // if price below lower band then buy
      if( tick.price < bbands.GetValue().lower ) {
        
        bt.SendOrder( 
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "long", idTrade ) 
        );
        state = ProcessingState::LONG;
        
      }
      // if price above apper band then sell
      if( tick.price > bbands.GetValue().upper ) {
        
        bt.SendOrder( 
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "short", idTrade ) 
        );
        state = ProcessingState::SHORT;
        
      }
      
    }
    // if strategy is long and price goes above sma then close long
    if( state == ProcessingState::LONG and tick.price > bbands.GetValue().sma ) {
      
      bt.SendOrder( 
        new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "close long", idTrade++ ) 
      );
      state = ProcessingState::FLAT;
      
    }
    // if strategy is below and price goes below sma then close long
    if( state == ProcessingState::SHORT and tick.price < bbands.GetValue().sma ) {
      
      bt.SendOrder( 
        new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "close short", idTrade++ ) 
      );
      state = ProcessingState::FLAT;
      
    }
    
    
  };
  
  // run back test on tick data
  bt.Feed( ticks );
  
  if( fast ) return bt.GetSummary();
  
  // combine candles and indicators history
  DataFrame indicators = ListBuilder()
    .Add( bt.GetCandles() )
    .Add( bbands .GetHistory() );
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}