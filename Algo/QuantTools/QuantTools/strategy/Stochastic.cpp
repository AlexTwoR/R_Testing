// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List stochastic(
    DataFrame ticks,
    int n,        // n of indicator
    int nFast,    // fast smooth
    int nSlow,    // slow smooth
    int timeFrame,    // candle priod in seconds
    double latency,   // round trip latency in seconds
    bool fast = false // return summary only
) {
  
  // define strategy states
  enum class ProcessingState{ LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;
  
  // initialize indicators
  Stochastic<double> stoch(n, nFast, nSlow);
  
  Crossover crossover;
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  bt.SetCost( cost );
  
  
  
  // define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    
    // add values to indicators
    stoch.Add( candle.close );

    
    // if stoch not formed yet do nothing
    if( not stoch.IsFormed()  ) return;
    
    // update crossover
    crossover.Add( std::pair< double, double >( stoch.GetValue().kFast, stoch.GetValue().dSlow ) );
    
    // if smaLong is above smaLong and current state is not long
    if( crossover.IsAbove() and state != ProcessingState::LONG ) {
      // if strategy has no position then buy
      if( state == ProcessingState::FLAT ) {
        bt.SendOrder(
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "long", idTrade )
        );
      }
      // if strategy has short position then close short position and open long position
      if( state == ProcessingState::SHORT ) {
        bt.SendOrder(
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "close short", idTrade++ )
        );
        bt.SendOrder(
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "reverse short", idTrade )
        );
      }
      // set state to long
      state = ProcessingState::LONG;
      
    }
    
    // same as above
    if( crossover.IsBelow() and state != ProcessingState::SHORT ) {
      
      if( state == ProcessingState::FLAT ) {
        bt.SendOrder(
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "short", idTrade )
        );
      }
      if( state == ProcessingState::LONG ) {
        bt.SendOrder(
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "close long", idTrade++ )
        );
        bt.SendOrder(
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "reverse long", idTrade )
        );
      }
      state = ProcessingState::SHORT;
      
    }
    
    
  };
  
  // run back test on tick data
  bt.Feed( ticks );
  
  if( fast ) return bt.GetSummary();
  
  // combine candles and indicators history
  DataFrame indicators = ListBuilder()
    .Add( bt.GetCandles() )
    .Add( "stoch_kFast", stoch.GetKFastHistory() )
    .Add( "stoch_dSlow", stoch.GetDSlowHistory() );
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}