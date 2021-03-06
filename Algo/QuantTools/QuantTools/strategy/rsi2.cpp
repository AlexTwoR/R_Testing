// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List rsi2( 
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
  //Rsi rsi( n );
  Crossover up_crossover;
  Crossover down_crossover;
  
  //EMA for Up/Down changer (RSI)
  Sma ema_up (n);
  Sma ema_down (n);
  
  //RSI Vars
  double sumGain = 0;
  double sumLoss = 0;
  double prevValue = NAN;
  double rsi = 0;
  std::vector< double > history;
  std::queue< double > window_up;
  std::queue< double > window_down;
  
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  bt.SetCost( cost );
  
  
  
  //define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    

    //-----RSI---
    if( std::isnan( prevValue ) ) prevValue = candle.close;
    double change = candle.close - prevValue;
    
    sumGain = 0.;
    sumLoss = 0.;
    
    change > 0 ? sumGain = change : sumLoss = -change;
    
    ema_up.Add(sumGain);
    ema_down.Add(sumLoss);
    
    // if moving averages not formed yet do nothing
    if( not ema_up.IsFormed() && not ema_down.IsFormed()) {
      history.push_back( NA_REAL );
      return;}
    
    //calc RSI
    rsi = 100. * ema_up.GetValue()/(ema_up.GetValue() + ema_down.GetValue());
    history.push_back( rsi );
    
    
    // update crossover
    up_crossover.Add( std::pair< double, double >( rsi, up ) );
    down_crossover.Add( std::pair< double, double >( rsi, down ) );
    
    if( down_crossover.IsBelow() and state != ProcessingState::LONG ) {
      
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
    
    
    if( up_crossover.IsAbove() and state != ProcessingState::SHORT ) {
      
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
    .Add( "rsi", history );
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}