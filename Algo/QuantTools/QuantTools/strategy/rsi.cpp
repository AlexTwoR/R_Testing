// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List rsi( 
    DataFrame ticks, 
    int n,            // RSI period
    double up,        // Up border
    double down,      // Down border
    int timeFrame,    // candle priod in seconds
    double latency,   // round trip latency in seconds
    bool fast = false // return summary only
) {
  
  // define strategy states
  enum class ProcessingState{ LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;
  
  // initialize indicators
  Rsi rsi( n );
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  bt.SetCost( cost );
  
  // define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    
    // add values to indicators
    rsi.Add( candle.close );
    
    // if RSI  not formed yet do nothing
    if( not rsi.IsFormed() ) return;
    

      //if RSI upper Up border - sell
      if(rsi.GetValue()>up){
        bt.SendOrder( 
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "short", idTrade ) 
        );
        state = ProcessingState::SHORT;
      }
      
      //if RSI lower Down border - buy
      if(rsi.GetValue()<down){
        bt.SendOrder( 
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "long", idTrade ) 
        );
        state = ProcessingState::LONG;
      }
    
    
  };

  
  // run back test on tick data
  bt.Feed( ticks );
  
  if( fast ) return bt.GetSummary();
  
  // combine candles and indicators history
  DataFrame indicators = ListBuilder()
    .Add( bt.GetCandles() )
    .Add( "rsi", rsi.GetHistory() );
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}