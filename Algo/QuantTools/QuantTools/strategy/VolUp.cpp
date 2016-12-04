// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List VolUp(
    DataFrame ticks,
    int n,   // Vol n
    int k,   // long sma n
    int shortPeriod,  // short sma n
    int timeFrame,    // candle priod in seconds
    double latency,   // round trip latency in seconds
    bool fast = false // return summary only
) {
  
  // define strategy states
  enum class ProcessingState{ LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;
  
  // initialize indicators

  Ema emaRet(n);
  RollSd sdRet(n);
  Crossover crossover;
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  bt.SetCost( cost );
  
  double ret = 0;
  
  // define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    
    ret = (candle.close-candle.open)/candle.open;
    emaRet.Add( ret );
    
    if( not emaRet.IsFormed() ) {
      sdRet.Add(0);
      return;
      }
    sdRet.Add( emaRet.GetValue() );
    
    if( not sdRet.IsFormed() ) return;
    
      
    // if smaLong is above smaLong and current state is not long
    if( emaRet.GetValue() < -k*sdRet.GetValue() && state != ProcessingState::LONG ) {
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
    

    if( emaRet.GetValue() > k*sdRet.GetValue() and state != ProcessingState::SHORT ) {
      
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
    .Add( "sdRet", sdRet.GetHistory() )
    .Add( "emaRet", emaRet.GetHistory() ); //sdRet
  
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}