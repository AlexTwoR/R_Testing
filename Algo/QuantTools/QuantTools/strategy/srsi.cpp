// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"
using namespace Rcpp;

// [[Rcpp::export]]
List StochRSI(
    DataFrame ticks,
    int n,  // n of RSI
    int k,  // k of min/max RSI
    int timeFrame,    // candle priod in seconds
    double latency,   // round trip latency in seconds
    bool fast = false // return summary only
) {
  
  // define strategy states
  enum class ProcessingState{ LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;
  
  Crossover crossover_zero;
  
  // initialize indicators
  Rsi rsi(n);
  Ema ema(k);
  RollRange rsi_range(k);
  
  double stoch_rsi = 0.0;
  double priceCorrection = 0;
  bool flag = true;
  Candle prevCandle( timeFrame );
  Sma effectivePriceHistory( 1 );
  
  //history
  std::vector< double > hist_stoch_rsi;
  
  
  // time params
  double current_time = 0.0;
  double start_workday = 9.7;
  double end_workday = 15.8;
  double previous_time = 0.0;
  
  // initialize Processor and set trading costs
  Processor bt( timeFrame, latency / 2, latency / 2 );
  Cost cost;
  cost.tradeAbs = -0.01;
  //cost.tradeRel = -0.2 / 10000;
  //cost.shortRel = 0.03 / 360;
  bt.SetCost( cost );
  
  // define what to do when new candle is formed
  bt.onCandle = [&]( Candle candle ) {
    
    if( !flag && candle.time - prevCandle.time > 60*60 ) 
      priceCorrection += candle.open - prevCandle.close;
    
    double effectiveClose = candle.close - priceCorrection;
    
    prevCandle = candle;
    flag = false;
    
    // add values to indicators
    rsi.Add( effectiveClose );
    ema.Add(rsi.GetValue());
    if( !rsi.IsFormed() ) return;
    
    
    
    rsi_range.Add(rsi.GetValue());
    if( !rsi_range.IsFormed()) return;
    
    // calc StochRSI
    stoch_rsi = ( rsi.GetValue() - rsi_range.GetValue().min )/( rsi_range.GetValue().max - rsi.GetValue() );
    
    crossover_zero.Add( std::pair< double, double >( stoch_rsi, 0.5 ) );
    hist_stoch_rsi.push_back(stoch_rsi);
    
  };
  
  bt.onTick = [&]( Tick tick ) {
    
    if( !rsi_range.IsFormed() || !rsi.IsFormed() ) return;
    
    current_time = std::fmod( tick.time / 3600, 24 );
    
    
    if(current_time > start_workday && current_time < end_workday){
      
      //return less sd -> Buy
      if( crossover_zero.IsAbove() && state != ProcessingState::LONG ) {
        
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
      
      
      //return more sd -> Sell
      if( crossover_zero.IsBelow() && state != ProcessingState::SHORT ) {
        
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
      
    } else {
      
      // close position at the end of the day
      if (state == ProcessingState::LONG){
        bt.SendOrder(
          new Order( OrderSide::SELL, OrderType::MARKET, NA_REAL, "close long", idTrade++)
        );
        state = ProcessingState::FLAT;
      }
      
      if (state == ProcessingState::SHORT){
        bt.SendOrder(
          new Order( OrderSide::BUY, OrderType::MARKET, NA_REAL, "close short", idTrade++ )
        );
        state = ProcessingState::FLAT;
      }
    }
    
    previous_time = (std::fmod( tick.time / 3600, 24 ));
    flag = 1;
    
    
    
  };
  
  // run back test on tick data
  bt.Feed( ticks );
  
  if( fast ) return bt.GetSummary();
  
  // combine candles and indicators history
  DataFrame indicators = ListBuilder()
    .Add( bt.GetCandles() )
    .Add( "EMA", ema.GetHistory() )
    .Add( "StochRSI", rsi.GetHistory());
  
  
  
  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"   , bt.GetSummary() )
    .Add( "trades"    , bt.GetTrades()  )
    .Add( "orders"    , bt.GetOrders()  )
    .Add( "indicators", indicators      );
  
}