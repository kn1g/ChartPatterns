#include <Rcpp.h>
using namespace Rcpp;

//' @name FastFindII_chaosRegin
//' @title Enhanced SHS and iSHS Pattern Detection with Chaos Region Analysis
//' @description Detects Shoulder-Head-Shoulder (SHS) and inverted Shoulder-Head-Shoulder (iSHS) patterns
//'              in financial time series with additional focus on chaos region dynamics.
//' @param PrePro_indexFilter Vector of indices identifying pivot points in the original data series
//' @param Original_times Vector with timestamps or indices for each data point
//' @param Original_prices Vector with price values corresponding to each timestamp
//' @return Returns a list containing:
//'    - patternInfo: DataFrame with pattern identification and trend information
//'    - Features2: DataFrame with pattern point timestamps and prices
//'    - Features21to40: DataFrame with return metrics
//' @details
//'   Tracks both valid and invalid pattern candidates using the validPattern flag.
//'   Analyzes trend information before and after the pattern formation.
//'   Calculates various return metrics at different time horizons.
//
//' @export
// [[Rcpp::export]]
List FastFindII_chaosRegin(IntegerVector PrePro_indexFilter,
              NumericVector Original_times,
              NumericVector Original_prices
){

    // Controls whether the index starts at zero
  if(PrePro_indexFilter[0] != 0){
    Function warning("warning");
    warning("PrePro Vector indices does not start at Zero.");
  }
  
  // Extract pivot points from the original dataset
  NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
  NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
  
  // Output containers
  std::vector<std::string> PatternName;      // Type of pattern (SHS or iSHS)
  std::vector<bool>        validPattern;     // Whether pattern has a valid breakout
  
  // Pattern position information
  std::vector<int>    firstIndexPrePro;      // Starting index in pivot point series
  std::vector<int>    firstindexOrigi;       // Starting index in original data
  std::vector<int>    breakoutIndex;         // Breakout index in original data
  
  // Pattern point timestamps
  std::vector<int>    timeStamp0;            // First point timestamp
  std::vector<int>    timeStamp1;            // Left shoulder timestamp
  std::vector<int>    timeStamp2;            // Left trough/peak timestamp
  std::vector<int>    timeStamp3;            // Head timestamp
  std::vector<int>    timeStamp4;            // Right trough/peak timestamp
  std::vector<int>    timeStamp5;            // Right shoulder timestamp
  std::vector<int>    timeStampBreakOut;     // Breakout timestamp
  
  // Pattern point prices
  std::vector<double> priceStamp0;           // First point price
  std::vector<double> priceStamp1;           // Left shoulder price
  std::vector<double> priceStamp2;           // Left trough/peak price
  std::vector<double> priceStamp3;           // Head price
  std::vector<double> priceStamp4;           // Right trough/peak price
  std::vector<double> priceStamp5;           // Right shoulder price
  std::vector<double> priceStampBreakOut;    // Breakout price
  
  // Trend information
  std::vector<double> TrendBeginnPreis;      // Price at trend beginning
  std::vector<int>    TrendBeginnZeit;       // Time at trend beginning
  std::vector<double> TrendEndePreis;        // Price at trend end
  std::vector<int>    TrendEndeZeit;         // Time at trend end
  
  // Fixed window returns
  std::vector<double> Rendite1V;             // Return after 1 time unit
  std::vector<double> Rendite3V;             // Return after 3 time units
  std::vector<double> Rendite5V;             // Return after 5 time units
  std::vector<double> Rendite10V;            // Return after 10 time units
  std::vector<double> Rendite30V;            // Return after 30 time units
  std::vector<double> Rendite60V;            // Return after 60 time units
  
  // Relative window returns
  std::vector<double> relRendite13V;         // Return after 1/3 of pattern length
  std::vector<double> relRendite12V;         // Return after 1/2 of pattern length
  std::vector<double> relRendite1V;          // Return after pattern length
  std::vector<double> relRendite2V;          // Return after 2x pattern length
  std::vector<double> relRendite4V;          // Return after 4x pattern length
  
  // Helper function for linear interpolation to calculate neckline values
  auto linInterp = [&](double x1, double x2, double y1, double y2, double x){
    if(std::fabs(x2 - x1) < 1e-15) return (y1+y2)/2.0;  // Fallback if x values are identical
    double slope = (y2 - y1)/(x2 - x1);
    return y1 + slope*(x - x1);
  };
  
  int n = QuerySeries_prices.size();
  if(n < 7) {
    // Not enough points for pattern detection, return empty results
    return List::create(
      _["patternInfo"]     = DataFrame::create(),
      _["Features2"]       = DataFrame::create(),
      _["Features21to40"]  = DataFrame::create()
    );
  }
  
  // Main loop
  for(int i = 0; i <= n - 7; i++){
    
    //--------------------------------------------------
    // SHS PATTERN DETECTION
    //--------------------------------------------------
    // Check if 7 points form a potential SHS (Shoulder-Head-Shoulder) pattern
    bool candidateSHS = (
      QuerySeries_prices[i]   < QuerySeries_prices[i+1] && // First point below left shoulder
      QuerySeries_prices[i]   < QuerySeries_prices[i+2] && // First point below left trough
      QuerySeries_prices[i+1] < QuerySeries_prices[i+3] && // Left shoulder below head
      QuerySeries_prices[i+5] < QuerySeries_prices[i+3] && // Right shoulder below head
      // Neckline relationship checks:
      QuerySeries_prices[i+5] > linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i+5]) && // Right shoulder above neckline
      QuerySeries_prices[i+1] > linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i+1]) && // Left shoulder above neckline
      // CRITICAL: Ensure first point is below neckline (prevents skewed patterns)
      QuerySeries_prices[i]   < linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i])
    );
    
    if(candidateSHS){
      //-----------------------------------
      // Initialize local variables for this pattern candidate
      //-----------------------------------
      bool localValid = false;  // Will become true if neckline is crossed properly
      
      // Pattern position indices
      int localFirstIndexPrePro = i+1;           // +1 for R's 1-based indexing
      int localFirstIndexOrigi  = PrePro_indexFilter[i] + 1;
      int localBreakoutIndex    = NA_INTEGER;    // Start as NA; fill if valid
      
      // Pattern point timestamps
      int    localTime0   = QuerySeries_times[i];      // First point timestamp
      int    localTime1   = QuerySeries_times[i+1];    // Left shoulder timestamp
      int    localTime2   = QuerySeries_times[i+2];    // Left trough timestamp
      int    localTime3   = QuerySeries_times[i+3];    // Head timestamp
      int    localTime4   = QuerySeries_times[i+4];    // Right trough timestamp
      int    localTime5   = QuerySeries_times[i+5];    // Right shoulder timestamp
      int    localTimeBO  = NA_INTEGER;               // Breakout timestamp (init as NA)
      
      // Pattern point prices
      double localPrice0  = QuerySeries_prices[i];     // First point price
      double localPrice1  = QuerySeries_prices[i+1];   // Left shoulder price
      double localPrice2  = QuerySeries_prices[i+2];   // Left trough price
      double localPrice3  = QuerySeries_prices[i+3];   // Head price
      double localPrice4  = QuerySeries_prices[i+4];   // Right trough price
      double localPrice5  = QuerySeries_prices[i+5];   // Right shoulder price
      double localPriceBO = NA_REAL;                  // Breakout price (init as NA)
      
      // Trend information
      double localBeginnPreis = NA_REAL;
      int    localBeginnZeit  = NA_INTEGER;
      double localEndePreis   = NA_REAL;
      int    localEndeZeit    = NA_INTEGER;
      
      // Return metrics (initialized as NA)
      double Rendite1  = NA_REAL;
      double Rendite3  = NA_REAL;
      double Rendite5  = NA_REAL;
      double Rendite10 = NA_REAL;
      double Rendite30 = NA_REAL;
      double Rendite60 = NA_REAL;
      
      double relRendite13 = NA_REAL;
      double relRendite12 = NA_REAL;
      double relRendite1  = NA_REAL;
      double relRendite2  = NA_REAL;
      double relRendite4  = NA_REAL;
      
      //-----------------------------------------
      // Search for pattern breakout (neckline crossing)
      //-----------------------------------------
      if(PrePro_indexFilter[i+5] < Original_times.size() - 1) {
        int jStart = PrePro_indexFilter[i+5];
        int originalSize = Original_times.size();
        
        for(int j = jStart; j < originalSize - 1; j++){
          
          // Pattern invalidation check: if price rises above right shoulder
          if(Original_prices[j] > localPrice5 && j != jStart){
            // No breakout possible, pattern is invalidated
            break;
          }
          
          // Calculate interpolated neckline value at current position
          double necklineY = linInterp(
            QuerySeries_times[i+2], QuerySeries_times[i+4],
            QuerySeries_prices[i+2], QuerySeries_prices[i+4],
            Original_times[j]
          );
          
          // Check for breakout: price dropping below neckline
          if(Original_prices[j] < necklineY){
            // Confirm breakout: next price also below right shoulder
            if(j+1 < originalSize && Original_prices[j+1] < localPrice5){
              // Valid pattern with confirmed breakout
              localValid = true;
              
              // Store breakout information
              localBreakoutIndex = j + 1;     // +1 for R indexing
              localTimeBO  = Original_times[j+1];
              localPriceBO = Original_prices[j+1];
              
              //-----------------------------------
              // Calculate preceding trend information
              //-----------------------------------
              // For SHS: Look for rising lows before pattern (bullish before bearish reversal)
              {
                double tbPreis = -1.0;
                int    tbZeit  = 99999991; // Sentinel value
                if(i > 2){
                  // Look backward in steps of 2 (low to low)
                  for(int rev = i; rev > 2; rev -= 2){
                    if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                      tbPreis = QuerySeries_prices[rev-2];
                      tbZeit  = QuerySeries_times[rev-2];
                    } else {
                      break;
                    }
                  }
                }
                localBeginnPreis = tbPreis;
                localBeginnZeit  = tbZeit;
              }
              
              //-----------------------------------
              // Calculate following trend information
              //-----------------------------------
              // For SHS: Look for falling highs after pattern (bearish continuation)
              {
                double tePreis = -1.0;
                int    teZeit  = 99999991;
                if(i + 5 < (n - 2)){
                  for(int forward = i+5; forward < (n - 2); forward += 2){
                    if(QuerySeries_prices[forward] > QuerySeries_prices[forward+2]){
                      tePreis = QuerySeries_prices[forward+2];
                      teZeit  = QuerySeries_times[forward+2];
                    } else {
                      break;
                    }
                  }
                }
                localEndePreis = tePreis;
                localEndeZeit  = teZeit;
              }
              
              //-----------------------------------
              // Calculate return metrics at different time horizons
              //-----------------------------------
              if(j >= (originalSize - 2)){
                // Not enough following data for return calculation
                // All return values remain NA
              } else {
                // Calculate time-based return windows
                int patternLengthInDays = Original_times[j+1] - localTime0; 
                int relDiff13 = patternLengthInDays / 3;     // 1/3 of pattern length
                int relDiff12 = patternLengthInDays / 2;     // 1/2 of pattern length
                int relDiff1  = patternLengthInDays;         // 1x pattern length
                int relDiff2  = patternLengthInDays * 2;     // 2x pattern length
                int relDiff4  = patternLengthInDays * 4;     // 4x pattern length
                
                // Search forward in original price series for return calculation points
                for(int forward = j+1; forward < (originalSize - 2); forward++){
                  int timeDiff = Original_times[forward] - Original_times[j+1];
                  
                  // Fixed time window returns (absolute periods)
                  if(timeDiff > 1  && R_IsNA(Rendite1))  { Rendite1   = Original_prices[forward]; }
                  if(timeDiff > 3  && R_IsNA(Rendite3))  { Rendite3   = Original_prices[forward]; }
                  if(timeDiff > 5  && R_IsNA(Rendite5))  { Rendite5   = Original_prices[forward]; }
                  if(timeDiff > 10 && R_IsNA(Rendite10)) { Rendite10  = Original_prices[forward]; }
                  if(timeDiff > 30 && R_IsNA(Rendite30)) { Rendite30  = Original_prices[forward]; }
                  if(timeDiff > 60 && R_IsNA(Rendite60)) { Rendite60  = Original_prices[forward]; }
                  
                  // Relative time window returns (based on pattern length)
                  if(timeDiff > relDiff13 && R_IsNA(relRendite13)) {
                    relRendite13 = Original_prices[forward];
                  }
                  if(timeDiff > relDiff12 && R_IsNA(relRendite12)) {
                    relRendite12 = Original_prices[forward];
                  }
                  if(timeDiff > relDiff1  && R_IsNA(relRendite1))  {
                    relRendite1  = Original_prices[forward];
                  }
                  if(timeDiff > relDiff2  && R_IsNA(relRendite2))  {
                    relRendite2  = Original_prices[forward];
                  }
                  if(timeDiff > relDiff4  && R_IsNA(relRendite4))  {
                    relRendite4  = Original_prices[forward];
                  }
                  
                  // Early exit once all returns are filled
                  if(!R_IsNA(relRendite4) || !R_IsNA(Rendite60)) {
                    break;
                  }
                }
              }
              
              // Valid breakout found, exit the loop
              break;
            }
          }
        } // end j loop
      } // end if safe to check breakout
      
      // Store pattern information regardless of validity (for all candidates)
      PatternName.push_back("SHS");
      validPattern.push_back(localValid);
      
      firstIndexPrePro.push_back(localFirstIndexPrePro);
      firstindexOrigi.push_back(localFirstIndexOrigi);
      breakoutIndex.push_back(localBreakoutIndex);
      
      timeStamp0.push_back(localTime0);
      timeStamp1.push_back(localTime1);
      timeStamp2.push_back(localTime2);
      timeStamp3.push_back(localTime3);
      timeStamp4.push_back(localTime4);
      timeStamp5.push_back(localTime5);
      timeStampBreakOut.push_back(localTimeBO);
      
      priceStamp0.push_back(localPrice0);
      priceStamp1.push_back(localPrice1);
      priceStamp2.push_back(localPrice2);
      priceStamp3.push_back(localPrice3);
      priceStamp4.push_back(localPrice4);
      priceStamp5.push_back(localPrice5);
      priceStampBreakOut.push_back(localPriceBO);
      
      TrendBeginnPreis.push_back(localBeginnPreis);
      TrendBeginnZeit.push_back(localBeginnZeit);
      TrendEndePreis.push_back(localEndePreis);
      TrendEndeZeit.push_back(localEndeZeit);
      
      Rendite1V.push_back(Rendite1);
      Rendite3V.push_back(Rendite3);
      Rendite5V.push_back(Rendite5);
      Rendite10V.push_back(Rendite10);
      Rendite30V.push_back(Rendite30);
      Rendite60V.push_back(Rendite60);
      
      relRendite13V.push_back(relRendite13);
      relRendite12V.push_back(relRendite12);
      relRendite1V.push_back(relRendite1);
      relRendite2V.push_back(relRendite2);
      relRendite4V.push_back(relRendite4);
    } // end if SHS candidate
    
    //--------------------------------------------------
    // iSHS PATTERN DETECTION (INVERSE SHS)
    //--------------------------------------------------
    // Check if 7 points form a potential iSHS (inverted Shoulder-Head-Shoulder) pattern
    bool candidateISHS = (
      QuerySeries_prices[i]   > QuerySeries_prices[i+1] && // First point above left shoulder
      QuerySeries_prices[i]   > QuerySeries_prices[i+2] && // First point above left peak
      QuerySeries_prices[i+1] > QuerySeries_prices[i+3] && // Left shoulder above head
      QuerySeries_prices[i+5] > QuerySeries_prices[i+3] && // Right shoulder above head
      // Neckline relationship checks:
      QuerySeries_prices[i+5] < linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i+5]) && // Right shoulder below neckline
      QuerySeries_prices[i+1] < linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i+1]) && // Left shoulder below neckline
      // CRITICAL: Ensure first point is above neckline (prevents skewed patterns)
      QuerySeries_prices[i]   > linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                        QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                        QuerySeries_times[i])
    );
    
    if(candidateISHS){
      //-----------------------------------
      // Initialize local variables for this pattern candidate
      //-----------------------------------
      bool localValid = false;  // Will become true if neckline is crossed properly
      
      // Pattern position indices
      int localFirstIndexPrePro = i+1;           // +1 for R's 1-based indexing
      int localFirstIndexOrigi  = PrePro_indexFilter[i] + 1;
      int localBreakoutIndex    = NA_INTEGER;    // Start as NA; fill if valid
      
      // Pattern point timestamps
      int    localTime0   = QuerySeries_times[i];      // First point timestamp
      int    localTime1   = QuerySeries_times[i+1];    // Left shoulder timestamp
      int    localTime2   = QuerySeries_times[i+2];    // Left peak timestamp
      int    localTime3   = QuerySeries_times[i+3];    // Head timestamp
      int    localTime4   = QuerySeries_times[i+4];    // Right peak timestamp
      int    localTime5   = QuerySeries_times[i+5];    // Right shoulder timestamp
      int    localTimeBO  = NA_INTEGER;               // Breakout timestamp (init as NA)
      
      // Pattern point prices
      double localPrice0  = QuerySeries_prices[i];     // First point price
      double localPrice1  = QuerySeries_prices[i+1];   // Left shoulder price
      double localPrice2  = QuerySeries_prices[i+2];   // Left peak price
      double localPrice3  = QuerySeries_prices[i+3];   // Head price
      double localPrice4  = QuerySeries_prices[i+4];   // Right peak price
      double localPrice5  = QuerySeries_prices[i+5];   // Right shoulder price
      double localPriceBO = NA_REAL;                  // Breakout price (init as NA)
      
      // Trend information
      double localBeginnPreis = NA_REAL;
      int    localBeginnZeit  = NA_INTEGER;
      double localEndePreis   = NA_REAL;
      int    localEndeZeit    = NA_INTEGER;
      
      // Return metrics (initialized as NA)
      double Rendite1  = NA_REAL;
      double Rendite3  = NA_REAL;
      double Rendite5  = NA_REAL;
      double Rendite10 = NA_REAL;
      double Rendite30 = NA_REAL;
      double Rendite60 = NA_REAL;
      
      double relRendite13 = NA_REAL;
      double relRendite12 = NA_REAL;
      double relRendite1  = NA_REAL;
      double relRendite2  = NA_REAL;
      double relRendite4  = NA_REAL;
      
      //-----------------------------------------
      // Search for pattern breakout (neckline crossing)
      //-----------------------------------------
      if(PrePro_indexFilter[i+5] < Original_times.size() - 1) {
        int jStart = PrePro_indexFilter[i+5];
        int originalSize = Original_times.size();
        
        for(int j = jStart; j < originalSize - 1; j++){
          
          // Pattern invalidation check: if price falls below right shoulder
          if(Original_prices[j] < localPrice5 && j != jStart){
            // No breakout possible, pattern is invalidated
            break;
          }
          
          // Calculate interpolated neckline value at current position
          double necklineY = linInterp(
            QuerySeries_times[i+2], QuerySeries_times[i+4],
            QuerySeries_prices[i+2], QuerySeries_prices[i+4],
            Original_times[j]
          );
          
          // Check for breakout: price rising above neckline
          if(Original_prices[j] > necklineY){
            // Confirm breakout: next price also above right shoulder
            if(j+1 < originalSize && Original_prices[j+1] > localPrice5){
              // Valid pattern with confirmed breakout
              localValid = true;
              
              // Store breakout information
              localBreakoutIndex = j + 1;     // +1 for R indexing
              localTimeBO  = Original_times[j+1];
              localPriceBO = Original_prices[j+1];
              
              //-----------------------------------
              // Calculate preceding trend information
              //-----------------------------------
              // For iSHS: Look for falling highs before pattern (bearish before bullish reversal)
              {
                double tbPreis = -1.0;
                int    tbZeit  = 99999991; // Sentinel value
                if(i > 2){
                  // Look backward in steps of 2 (high to high)
                  for(int rev = i; rev > 2; rev -= 2){
                    if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                      tbPreis = QuerySeries_prices[rev-2];
                      tbZeit  = QuerySeries_times[rev-2];
                    } else {
                      break;
                    }
                  }
                }
                localBeginnPreis = tbPreis;
                localBeginnZeit  = tbZeit;
              }
              
              //-----------------------------------
              // Calculate following trend information
              //-----------------------------------
              // For iSHS: Look for rising lows after pattern (bullish continuation)
              {
                double tePreis = -1.0;
                int    teZeit  = 99999991;
                if(i + 5 < (n - 2)){
                  for(int forward = i+5; forward < (n - 2); forward += 2){
                    if(QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                      tePreis = QuerySeries_prices[forward+2];
                      teZeit  = QuerySeries_times[forward+2];
                    } else {
                      break;
                    }
                  }
                }
                localEndePreis = tePreis;
                localEndeZeit  = teZeit;
              }
              
              //-----------------------------------
              // Calculate return metrics at different time horizons
              //-----------------------------------
              if(j >= (originalSize - 2)){
                // Not enough following data for return calculation
                // All return values remain NA
              } else {
                // Calculate time-based return windows
                int patternLengthInDays = Original_times[j+1] - localTime0; 
                int relDiff13 = patternLengthInDays / 3;     // 1/3 of pattern length
                int relDiff12 = patternLengthInDays / 2;     // 1/2 of pattern length
                int relDiff1  = patternLengthInDays;         // 1x pattern length
                int relDiff2  = patternLengthInDays * 2;     // 2x pattern length
                int relDiff4  = patternLengthInDays * 4;     // 4x pattern length
                
                // Search forward in original price series for return calculation points
                for(int forward = j+1; forward < (originalSize - 2); forward++){
                  int timeDiff = Original_times[forward] - Original_times[j+1];
                  
                  // Fixed time window returns (logarithmic/ratio for iSHS)
                  if(timeDiff > 1  && R_IsNA(Rendite1))  { Rendite1   = log(Original_prices[forward] / Original_prices[j+1]); }
                  if(timeDiff > 3  && R_IsNA(Rendite3))  { Rendite3   = Original_prices[forward] / Original_prices[j+1]; }
                  if(timeDiff > 5  && R_IsNA(Rendite5))  { Rendite5   = Original_prices[forward] / Original_prices[j+1]; }
                  if(timeDiff > 10 && R_IsNA(Rendite10)) { Rendite10  = Original_prices[forward] / Original_prices[j+1]; }
                  if(timeDiff > 30 && R_IsNA(Rendite30)) { Rendite30  = Original_prices[forward] / Original_prices[j+1]; }
                  if(timeDiff > 60 && R_IsNA(Rendite60)) { Rendite60  = Original_prices[forward] / Original_prices[j+1]; }
                  
                  // Relative time window returns (ratio-based for iSHS)
                  if(timeDiff > relDiff13 && R_IsNA(relRendite13)) {
                    relRendite13 = Original_prices[forward] / Original_prices[j+1];
                  }
                  if(timeDiff > relDiff12 && R_IsNA(relRendite12)) {
                    relRendite12 = Original_prices[forward] / Original_prices[j+1];
                  }
                  if(timeDiff > relDiff1  && R_IsNA(relRendite1))  {
                    relRendite1  = Original_prices[forward] / Original_prices[j+1];
                  }
                  if(timeDiff > relDiff2  && R_IsNA(relRendite2))  {
                    relRendite2  = Original_prices[forward] / Original_prices[j+1];
                  }
                  if(timeDiff > relDiff4  && R_IsNA(relRendite4))  {
                    relRendite4  = Original_prices[forward] / Original_prices[j+1];
                  }
                  
                  // Early exit once all returns are filled
                  if(!R_IsNA(relRendite4) || !R_IsNA(Rendite60)) {
                    break;
                  }
                }
              }
              
              // Valid breakout found, exit the loop
              break;
            }
          }
        } // end j loop
      } // end if safe to check breakout
      
      // Now push back exactly once for this candidate
      PatternName.push_back("iSHS");
      validPattern.push_back(localValid);
      
      firstIndexPrePro.push_back(localFirstIndexPrePro);
      firstindexOrigi.push_back(localFirstIndexOrigi);
      breakoutIndex.push_back(localBreakoutIndex);
      
      timeStamp0.push_back(localTime0);
      timeStamp1.push_back(localTime1);
      timeStamp2.push_back(localTime2);
      timeStamp3.push_back(localTime3);
      timeStamp4.push_back(localTime4);
      timeStamp5.push_back(localTime5);
      timeStampBreakOut.push_back(localTimeBO);
      
      priceStamp0.push_back(localPrice0);
      priceStamp1.push_back(localPrice1);
      priceStamp2.push_back(localPrice2);
      priceStamp3.push_back(localPrice3);
      priceStamp4.push_back(localPrice4);
      priceStamp5.push_back(localPrice5);
      priceStampBreakOut.push_back(localPriceBO);
      
      TrendBeginnPreis.push_back(localBeginnPreis);
      TrendBeginnZeit.push_back(localBeginnZeit);
      TrendEndePreis.push_back(localEndePreis);
      TrendEndeZeit.push_back(localEndeZeit);
      
      Rendite1V.push_back(Rendite1);
      Rendite3V.push_back(Rendite3);
      Rendite5V.push_back(Rendite5);
      Rendite10V.push_back(Rendite10);
      Rendite30V.push_back(Rendite30);
      Rendite60V.push_back(Rendite60);
      
      relRendite13V.push_back(relRendite13);
      relRendite12V.push_back(relRendite12);
      relRendite1V.push_back(relRendite1);
      relRendite2V.push_back(relRendite2);
      relRendite4V.push_back(relRendite4);
    } // end if iSHS candidate
    
  } // end main i loop
  
  //--------------------------------------------
  // Construct final DataFrame and List outputs
  //--------------------------------------------
  
  // Basic pattern information and trend context
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
    Named("PatternName")          = PatternName,        // Type of pattern (SHS or iSHS)
    Named("validPattern")         = validPattern,       // Whether pattern has a valid breakout
    Named("firstIndexinPrePro")   = firstIndexPrePro,   // Starting index in pivot point series
    Named("firstIndexinOriginal") = firstindexOrigi,    // Starting index in original data
    Named("breakoutIndexinOrig")  = breakoutIndex,      // Breakout index in original data
    Named("TrendBeginnPreis")     = TrendBeginnPreis,   // Price at trend beginning
    Named("TrendBeginnZeit")      = TrendBeginnZeit,    // Time at trend beginning
    Named("TrendEndePreis")       = TrendEndePreis,     // Price at trend end
    Named("TrendEndeZeit")        = TrendEndeZeit       // Time at trend end
  );
  
  // Pattern point timestamps and prices for visualization
  Rcpp::DataFrame Features2 = Rcpp::DataFrame::create(
    Named("timeStamp0")         = timeStamp0,           // First point timestamp
    Named("timeStamp1")         = timeStamp1,           // Left shoulder timestamp
    Named("timeStamp2")         = timeStamp2,           // Left trough/peak timestamp
    Named("timeStamp3")         = timeStamp3,           // Head timestamp
    Named("timeStamp4")         = timeStamp4,           // Right trough/peak timestamp
    Named("timeStamp5")         = timeStamp5,           // Right shoulder timestamp
    Named("timeStampBreakOut")  = timeStampBreakOut,    // Breakout timestamp
    Named("priceStamp0")        = priceStamp0,          // First point price
    Named("priceStamp1")        = priceStamp1,          // Left shoulder price
    Named("priceStamp2")        = priceStamp2,          // Left trough/peak price
    Named("priceStamp3")        = priceStamp3,          // Head price
    Named("priceStamp4")        = priceStamp4,          // Right trough/peak price
    Named("priceStamp5")        = priceStamp5,          // Right shoulder price
    Named("priceStampBreakOut") = priceStampBreakOut    // Breakout price
  );
  
  // Return metrics for pattern performance analysis
  Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
    Named("Rendite1V")     = Rendite1V,      // Return 1 time unit after breakout
    Named("Rendite3V")     = Rendite3V,      // Return 3 time units after breakout
    Named("Rendite5V")     = Rendite5V,      // Return 5 time units after breakout
    Named("Rendite10V")    = Rendite10V,     // Return 10 time units after breakout
    Named("Rendite30V")    = Rendite30V,     // Return 30 time units after breakout
    Named("Rendite60V")    = Rendite60V,     // Return 60 time units after breakout
    Named("relRendite13V") = relRendite13V,  // Return after 1/3 of pattern length
    Named("relRendite12V") = relRendite12V,  // Return after 1/2 of pattern length
    Named("relRendite1V")  = relRendite1V,   // Return after 1x pattern length
    Named("relRendite2V")  = relRendite2V,   // Return after 2x pattern length
    Named("relRendite4V")  = relRendite4V    // Return after 4x pattern length
  );
  
  // Return all DataFrames as a named List
  return Rcpp::List::create(
    Named("patternInfo")    = patternInfo,     // Basic pattern information
    Named("Features2")      = Features2,       // Pattern point details
    Named("Features21to40") = Features21to41   // Return metrics
  );
}
