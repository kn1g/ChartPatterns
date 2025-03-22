#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List FastFindII_chaosRegin(IntegerVector PrePro_indexFilter,
              NumericVector Original_times,
              NumericVector Original_prices
){
  
  NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
  NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
  
  // Output containers
  std::vector<std::string> PatternName;
  std::vector<bool>        validPattern;
  
  // Indices, times, prices, etc.
  std::vector<int>    firstIndexPrePro;
  std::vector<int>    firstindexOrigi;
  std::vector<int>    breakoutIndex;
  
  std::vector<int>    timeStamp0;
  std::vector<int>    timeStamp1;
  std::vector<int>    timeStamp2;
  std::vector<int>    timeStamp3;
  std::vector<int>    timeStamp4;
  std::vector<int>    timeStamp5;
  std::vector<int>    timeStampBreakOut;
  
  std::vector<double> priceStamp0;
  std::vector<double> priceStamp1;
  std::vector<double> priceStamp2;
  std::vector<double> priceStamp3;
  std::vector<double> priceStamp4;
  std::vector<double> priceStamp5;
  std::vector<double> priceStampBreakOut;
  
  // Trend
  std::vector<double> TrendBeginnPreis;
  std::vector<int>    TrendBeginnZeit;
  std::vector<double> TrendEndePreis;
  std::vector<int>    TrendEndeZeit;
  
  // Returns
  std::vector<double> Rendite1V;
  std::vector<double> Rendite3V;
  std::vector<double> Rendite5V;
  std::vector<double> Rendite10V;
  std::vector<double> Rendite30V;
  std::vector<double> Rendite60V;
  
  std::vector<double> relRendite13V;
  std::vector<double> relRendite12V;
  std::vector<double> relRendite1V;
  std::vector<double> relRendite2V;
  std::vector<double> relRendite4V;
  
  // Helper for linear interpolation
  auto linInterp = [&](double x1, double x2, double y1, double y2, double x){
    if(std::fabs(x2 - x1) < 1e-15) return (y1+y2)/2.0;  // fallback if x2==x1
    double slope = (y2 - y1)/(x2 - x1);
    return y1 + slope*(x - x1);
  };
  
  int n = QuerySeries_prices.size();
  if(n < 7) {
    // Not enough points
    // Return empty results
    return List::create(
      _["patternInfo"]     = DataFrame::create(),
      _["Features2"]       = DataFrame::create(),
      _["Features21to40"]  = DataFrame::create()
    );
  }
  
  // Main loop
  for(int i = 0; i <= n - 6; i++){
    
    // Check if 7 points form a potential SHS
    bool candidateSHS = (
      QuerySeries_prices[i]   < QuerySeries_prices[i+1] &&
        QuerySeries_prices[i]   < QuerySeries_prices[i+2] &&
        QuerySeries_prices[i+1] < QuerySeries_prices[i+3] &&
        QuerySeries_prices[i+5] < QuerySeries_prices[i+3] &&
        QuerySeries_prices[i+5] > linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                            QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                                                                       QuerySeries_times[i+5]) &&
                                                                                         QuerySeries_prices[i+1] > linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                                                                                                             QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                                                                                                                                                        QuerySeries_times[i+1]) &&
                                                                                                                                                                          QuerySeries_prices[i]   < linInterp(QuerySeries_times[i+2], QuerySeries_times[i+4],
                                                                                                                                                                                                              QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                                                                                                                                                                                                                                         QuerySeries_times[i])
    );
    
    if(!candidateSHS){
      // Not even a candidate; skip
      continue;
    }
    
    //-----------------------------------
    // Prepare local variables for output
    //-----------------------------------
    bool localValid = false;  // Will become true if neckline is crossed properly
    
    // Basic indexing
    int localFirstIndexPrePro = i+1;           // +1 for R's 1-based indexing
    int localFirstIndexOrigi  = PrePro_indexFilter[i] + 1;
    int localBreakoutIndex    = NA_INTEGER;    // Start as NA; fill if valid
    
    // Time / Price stamps
    int    localTime0   = QuerySeries_times[i];
    int    localTime1   = QuerySeries_times[i+1];
    int    localTime2   = QuerySeries_times[i+2];
    int    localTime3   = QuerySeries_times[i+3];
    int    localTime4   = QuerySeries_times[i+4];
    int    localTime5   = QuerySeries_times[i+5];
    int    localTimeBO  = NA_INTEGER;         // BreakOut time
    
    double localPrice0  = QuerySeries_prices[i];
    double localPrice1  = QuerySeries_prices[i+1];
    double localPrice2  = QuerySeries_prices[i+2];
    double localPrice3  = QuerySeries_prices[i+3];
    double localPrice4  = QuerySeries_prices[i+4];
    double localPrice5  = QuerySeries_prices[i+5];
    double localPriceBO = NA_REAL;            // BreakOut price
    
    // Trend
    double localBeginnPreis = NA_REAL;
    int    localBeginnZeit  = NA_INTEGER;
    double localEndePreis   = NA_REAL;
    int    localEndeZeit    = NA_INTEGER;
    
    // Returns
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
    // Attempt to find the neckline breakout
    //-----------------------------------------
    int jStart = PrePro_indexFilter[i+5];
    int originalSize = Original_times.size();
    for(int j = jStart; j < originalSize - 1; j++){
      
      // if the original Prices rise above the right shoulder we can stop => invalid
      if(Original_prices[j] > localPrice5 && j != jStart){
        // No breakout possible => break
        break;
      }
      
      // Check if neckline is crossed
      double necklineY = linInterp(
        QuerySeries_times[i+2], QuerySeries_times[i+4],
                                                 QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                                                                                            Original_times[j]
      );
      
      // If price is below neckline => potential breakout
      if(Original_prices[j] < necklineY){
        // Check if next price is still under the right shoulder
        if(Original_prices[j+1] < localPrice5){
          // => valid pattern
          localValid = true;
          
          // Fill breakout info
          localBreakoutIndex = j + 1;     // +1 for R indexing
          localTimeBO  = Original_times[j+1];
          localPriceBO = Original_prices[j+1];
          
          //-----------------------------------
          // Trend calculation
          //-----------------------------------
          // Preceding trend
          {
            double tbPreis = -1.0;
            int    tbZeit  = 99999991; // sentinel
            if(i > 2){
              // Look backward in steps of 2
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
          
          // Forward trend
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
          // Calculate returns
          //-----------------------------------
          if(j >= (originalSize - 2)){
            // Not enough data => everything stays NA
          } else {
            // Time-based windows
            int patternLengthInDays = Original_times[j+1] - localTime0; // or localTime0
            int relDiff13 = patternLengthInDays / 3;
            int relDiff12 = patternLengthInDays / 2;
            int relDiff1  = patternLengthInDays;
            int relDiff2  = patternLengthInDays * 2;
            int relDiff4  = patternLengthInDays * 4;
            
            // Search forward in Original_prices
            for(int forward = j+1; forward < (originalSize - 2); forward++){
              int timeDiff = Original_times[forward] - Original_times[j+1];
              if(timeDiff > 1  && R_IsNA(Rendite1))  { Rendite1   = Original_prices[forward]; }
              if(timeDiff > 3  && R_IsNA(Rendite3))  { Rendite3   = Original_prices[forward]; }
              if(timeDiff > 5  && R_IsNA(Rendite5))  { Rendite5   = Original_prices[forward]; }
              if(timeDiff > 10 && R_IsNA(Rendite10)) { Rendite10  = Original_prices[forward]; }
              if(timeDiff > 30 && R_IsNA(Rendite30)) { Rendite30  = Original_prices[forward]; }
              if(timeDiff > 60 && R_IsNA(Rendite60)) { Rendite60  = Original_prices[forward]; }
              
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
              
              // Early exit if all returns are filled
              if(!R_IsNA(relRendite4) || !R_IsNA(Rendite60)) {
                break;
              }
            }
          }
          
          // We found a valid breakout => break from the j loop
          break;
        }
      }
    } // end j loop
    
    // Now push back exactly once for this candidate
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
    
  } // end main i loop
  
  //--------------------------------------------
  // Construct final DataFrame or List outputs
  //--------------------------------------------
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
    Named("PatternName")          = PatternName,
    Named("validPattern")         = validPattern,
    Named("firstIndexinPrePro")   = firstIndexPrePro,
    Named("firstIndexinOriginal") = firstindexOrigi,
    Named("breakoutIndexinOrig")  = breakoutIndex,
    Named("TrendBeginnPreis")     = TrendBeginnPreis,
    Named("TrendBeginnZeit")      = TrendBeginnZeit,
    Named("TrendEndePreis")       = TrendEndePreis,
    Named("TrendEndeZeit")        = TrendEndeZeit
  );
  
  Rcpp::DataFrame Features2 = Rcpp::DataFrame::create(
    Named("timeStamp0")         = timeStamp0,
    Named("timeStamp1")         = timeStamp1,
    Named("timeStamp2")         = timeStamp2,
    Named("timeStamp3")         = timeStamp3,
    Named("timeStamp4")         = timeStamp4,
    Named("timeStamp5")         = timeStamp5,
    Named("timeStampBreakOut")  = timeStampBreakOut,
    Named("priceStamp0")        = priceStamp0,
    Named("priceStamp1")        = priceStamp1,
    Named("priceStamp2")        = priceStamp2,
    Named("priceStamp3")        = priceStamp3,
    Named("priceStamp4")        = priceStamp4,
    Named("priceStamp5")        = priceStamp5,
    Named("priceStampBreakOut") = priceStampBreakOut
  );
  
  Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
    Named("Rendite1V")     = Rendite1V,
    Named("Rendite3V")     = Rendite3V,
    Named("Rendite5V")     = Rendite5V,
    Named("Rendite10V")    = Rendite10V,
    Named("Rendite30V")    = Rendite30V,
    Named("Rendite60V")    = Rendite60V,
    Named("relRendite13V") = relRendite13V,
    Named("relRendite12V") = relRendite12V,
    Named("relRendite1V")  = relRendite1V,
    Named("relRendite2V")  = relRendite2V,
    Named("relRendite4V")  = relRendite4V
  );
  
  return Rcpp::List::create(
    Named("patternInfo")    = patternInfo,
    Named("Features2")      = Features2,
    Named("Features21to40") = Features21to41
  );
}
