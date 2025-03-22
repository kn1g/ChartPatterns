#include <vector>
#include <string>
#include <cmath>
#include "cppHeader.hpp"
#include <Eigen/Dense>

/**
 * @file FastFind.cpp
 * @brief Implements fast pattern recognition for financial chart patterns
 * 
 * This file provides implementation for detecting chart patterns in financial time series data.
 * Currently supports Shoulder-Head-Shoulder (SHS) and inverse Shoulder-Head-Shoulder (iSHS) patterns.
 * The implementation uses preprocessed pivot points to efficiently identify potential patterns.
 */

// Forward declarations
double linearInterpolation(double x1, double x2, double y1, double y2, double x);
bool isValidIndex(int idx, int maxSize);
bool detectPattern(const NumericVector& prices, const NumericVector& times, int i, 
                  bool isInverted, double& leftNecklineValue, double& rightNecklineValue, 
                  double& firstPointNecklineValue);
void calculateReturns(const NumericVector& prices, const NumericVector& times,
                     int breakoutIdx, int patternStartIdx, std::vector<double>& returns,
                     std::vector<double>& relReturns);

// Constants for optimization
const int EXPECTED_PATTERN_COUNT = 100;  // Reasonable guess for pre-allocation
const double MIN_HEAD_SHOULDER_DIFF = 0.01; // Minimum price difference to consider a valid head-shoulder pattern
const int MAX_LOOK_AHEAD = 60; // Maximum periods to check for return calculations

// Constants like 99999991 appear in the code
const int INVALID_TIME = 99999991;

// Define a common interface for all pattern detectors
class PatternDetector {
public:
    virtual ~PatternDetector() = default;
    
    // Returns true if a pattern is detected at the given position
    virtual bool detect(const NumericVector& prices, const NumericVector& times, 
                        int position, PatternData& outPattern) const = 0;
    
    // Each detector knows how to check for its breakout condition
    virtual bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                               int j, const PatternData& pattern) const = 0;
                               
    // Get the name of this pattern
    virtual std::string getName() const = 0;
};

class SHSDetector : public PatternDetector {
public:
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override {
        // SHS-specific detection logic
        // ...
        return patternDetected;
    }
    
    bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                       int j, const PatternData& pattern) const override {
        // SHS-specific breakout detection
        // ...
        return breakoutDetected;
    }
    
    std::string getName() const override {
        return "SHS";
    }
};

class ISHSDetector : public PatternDetector {
    // Similar implementation for inverse SHS pattern
};

//' @name fastFind
 //' @title fastFind Patterns
//' @description The pattern recognition is done for all patterns in one loop. The single functions loop per pattern over the dataset
 //' @param prices Vector with prices
//' @param time Vector with time or indices
 //' @param mask with PIPs in the price-time vectors
 //' @return Returns First the index where a pattern is located
 //' @examples
 //' c(1:10)
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::DataFrame fastFind(IntegerVector PrePro_indexFilter,
                          NumericVector Original_times,
                          NumericVector Original_prices
 ){
   
  // Controls whether the index starts at zero
  if(PrePro_indexFilter[0] != 0){
    Function warning("warning");
    warning("PrePro Vector indices does not start at Zero.");
  }
  
  // Find PIPs (Price Inflection Points) in the original dataset
   NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
   NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
   
  // Create a struct to hold pattern data
  struct PatternData {
    int startIdx, leftShoulderIdx, necklineStartIdx, headIdx, necklineEndIdx, rightShoulderIdx, breakoutIdx;
    std::string patternName;
    std::vector<int> timeStamps;
    std::vector<double> priceStamps;
    double trendBeginPrice;
    int trendBeginTime;
    double trendEndPrice;
    int trendEndTime;
    std::vector<double> returns;       // Fixed time window returns
    std::vector<double> relReturns;    // Relative time window returns
  };
  
  // Use a vector of this struct instead of separate vectors
  std::vector<PatternData> patterns;
  
  // Pre-allocate with a reasonable capacity to reduce reallocations
  patterns.reserve(EXPECTED_PATTERN_COUNT);
  
  // Main loop through data to find patterns
  // We need at least 6 points for pattern detection, thus -6 in the loop condition
  std::vector<std::unique_ptr<PatternDetector>> detectors;
  detectors.push_back(std::make_unique<SHSDetector>());
  detectors.push_back(std::make_unique<ISHSDetector>());
  // Add more detectors as needed
  
  for(int i=0; i < (QuerySeries_prices.size() - 6); ++i){
    
    // Add early rejection conditions before the expensive pattern checks
    if (abs(QuerySeries_prices[i+3] - QuerySeries_prices[i+1]) < MIN_HEAD_SHOULDER_DIFF ||
        abs(QuerySeries_prices[i+3] - QuerySeries_prices[i+5]) < MIN_HEAD_SHOULDER_DIFF) {
        continue; // Skip if head isn't significantly different from shoulders
    }
    
    // Pre-calculate neckline values (reused for both pattern types)
    double leftShoulderNecklineValue = 0;
    double rightShoulderNecklineValue = 0;
    double firstPointNecklineValue = 0;
    
    bool isInverted = false; // Start with SHS
    for (int attempt = 0; attempt < 2; attempt++) {
        for(const auto& detector : detectors) {
            PatternData pattern;
            if(detector->detect(QuerySeries_prices, QuerySeries_times, i, pattern)) {
                // Process detected pattern...
                // Breakout detection logic using detector->detectBreakout()
                // ...
                patterns.push_back(pattern);
            }
        }
        isInverted = true; // Try iSHS on second attempt
    }
  }
  
  // Now convert the pattern data back to the format expected by R
  // Extract data from patterns to create the flat vectors
  std::vector<std::string> PatternName;
  std::vector<int> startIdx;
  std::vector<int> leftShoulderIdx;
  std::vector<int> necklineStartIdx;
  std::vector<int> headIdx;
  std::vector<int> necklineEndIdx;
  std::vector<int> rightShoulderIdx;
  std::vector<int> breakoutIdx;
  
  std::vector<int> timeStamp0, timeStamp1, timeStamp2, timeStamp3, timeStamp4, timeStamp5, timeStampBreakOut;
  std::vector<double> priceStamp0, priceStamp1, priceStamp2, priceStamp3, priceStamp4, priceStamp5, priceStampBreakOut;
  
  std::vector<double> TrendBeginnPreis;
  std::vector<int> TrendBeginnZeit;
  std::vector<double> TrendEndePreis;
  std::vector<int> TrendEndeZeit;
  
  std::vector<double> Rendite1V, Rendite3V, Rendite5V, Rendite10V, Rendite30V, Rendite60V;
  std::vector<double> relRendite13V, relRendite12V, relRendite1V, relRendite2V, relRendite4V;
  
  // Pre-allocate vectors
  int patternCount = patterns.size();
  PatternName.reserve(patternCount);
  startIdx.reserve(patternCount);
  leftShoulderIdx.reserve(patternCount);
  necklineStartIdx.reserve(patternCount);
  headIdx.reserve(patternCount);
  necklineEndIdx.reserve(patternCount);
  rightShoulderIdx.reserve(patternCount);
  breakoutIdx.reserve(patternCount);
  
  timeStamp0.reserve(patternCount);
  timeStamp1.reserve(patternCount);
  timeStamp2.reserve(patternCount);
  timeStamp3.reserve(patternCount);
  timeStamp4.reserve(patternCount);
  timeStamp5.reserve(patternCount);
  timeStampBreakOut.reserve(patternCount);
  
  priceStamp0.reserve(patternCount);
  priceStamp1.reserve(patternCount);
  priceStamp2.reserve(patternCount);
  priceStamp3.reserve(patternCount);
  priceStamp4.reserve(patternCount);
  priceStamp5.reserve(patternCount);
  priceStampBreakOut.reserve(patternCount);
  
  TrendBeginnPreis.reserve(patternCount);
  TrendBeginnZeit.reserve(patternCount);
  TrendEndePreis.reserve(patternCount);
  TrendEndeZeit.reserve(patternCount);
  
  Rendite1V.reserve(patternCount);
  Rendite3V.reserve(patternCount);
  Rendite5V.reserve(patternCount);
  Rendite10V.reserve(patternCount);
  Rendite30V.reserve(patternCount);
  Rendite60V.reserve(patternCount);
  
  relRendite13V.reserve(patternCount);
  relRendite12V.reserve(patternCount);
  relRendite1V.reserve(patternCount);
  relRendite2V.reserve(patternCount);
  relRendite4V.reserve(patternCount);
  
  // Extract data from each pattern
  for(const auto& pattern : patterns) {
    PatternName.push_back(pattern.patternName);
    startIdx.push_back(pattern.startIdx);
    leftShoulderIdx.push_back(pattern.leftShoulderIdx);
    necklineStartIdx.push_back(pattern.necklineStartIdx);
    headIdx.push_back(pattern.headIdx);
    necklineEndIdx.push_back(pattern.necklineEndIdx);
    rightShoulderIdx.push_back(pattern.rightShoulderIdx);
    breakoutIdx.push_back(pattern.breakoutIdx);
    
    // Time stamps
    timeStamp0.push_back(pattern.timeStamps[0]);
    timeStamp1.push_back(pattern.timeStamps[1]);
    timeStamp2.push_back(pattern.timeStamps[2]);
    timeStamp3.push_back(pattern.timeStamps[3]);
    timeStamp4.push_back(pattern.timeStamps[4]);
    timeStamp5.push_back(pattern.timeStamps[5]);
    timeStampBreakOut.push_back(pattern.timeStamps[6]);
    
    // Price stamps
    priceStamp0.push_back(pattern.priceStamps[0]);
    priceStamp1.push_back(pattern.priceStamps[1]);
    priceStamp2.push_back(pattern.priceStamps[2]);
    priceStamp3.push_back(pattern.priceStamps[3]);
    priceStamp4.push_back(pattern.priceStamps[4]);
    priceStamp5.push_back(pattern.priceStamps[5]);
    priceStampBreakOut.push_back(pattern.priceStamps[6]);
    
    // Trend data
    TrendBeginnPreis.push_back(pattern.trendBeginPrice);
    TrendBeginnZeit.push_back(pattern.trendBeginTime);
    TrendEndePreis.push_back(pattern.trendEndPrice);
    TrendEndeZeit.push_back(pattern.trendEndTime);
    
    // Returns data - fixed windows
    Rendite1V.push_back(pattern.returns[0]);
    Rendite3V.push_back(pattern.returns[1]);
    Rendite5V.push_back(pattern.returns[2]);
    Rendite10V.push_back(pattern.returns[3]);
    Rendite30V.push_back(pattern.returns[4]);
    Rendite60V.push_back(pattern.returns[5]);
    
    // Returns data - relative windows
    relRendite13V.push_back(pattern.relReturns[0]);
    relRendite12V.push_back(pattern.relReturns[1]);
    relRendite1V.push_back(pattern.relReturns[2]);
    relRendite2V.push_back(pattern.relReturns[3]);
    relRendite4V.push_back(pattern.relReturns[4]);
  }
  
  // RCPP cannot handle data frames with more than 20 columns.
  // We need to split the information and then put it together in a LIST
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
    Rcpp::Named("PatternName")              = PatternName,
                                                         Rcpp::Named("startIdx")       = startIdx,
                                                         Rcpp::Named("leftShoulderIdx")     = leftShoulderIdx,
                                                         Rcpp::Named("necklineStartIdx")     = necklineStartIdx,
                                                         Rcpp::Named("headIdx")          = headIdx,
                                                         Rcpp::Named("necklineEndIdx")      = necklineEndIdx,
                                                         Rcpp::Named("rightShoulderIdx")      = rightShoulderIdx,
                                                         Rcpp::Named("breakoutIdx")      = breakoutIdx,
                                                         Rcpp::Named("TrendBeginnPreis")         = TrendBeginnPreis,
                                                         Rcpp::Named("TrendBeginnZeit")          = TrendBeginnZeit,
                                                         Rcpp::Named("TrendEndePreis")           = TrendEndePreis,
                                                         Rcpp::Named("TrendEndeZeit")            = TrendEndeZeit
   );
   
  Rcpp::DataFrame Features2 = Rcpp::DataFrame::create(
    Rcpp::Named("timeStamp0")           = timeStamp0,
                                                            Rcpp::Named("timeStamp1")           = timeStamp1,
                                                            Rcpp::Named("timeStamp2")           = timeStamp2,
                                                            Rcpp::Named("timeStamp3")           = timeStamp3,
                                                            Rcpp::Named("timeStamp4")           = timeStamp4,
                                                            Rcpp::Named("timeStamp5")           = timeStamp5,
                                                            Rcpp::Named("timeStampBreakOut")    = timeStampBreakOut,
                                                            Rcpp::Named("priceStamp0")          = priceStamp0,
                                                            Rcpp::Named("priceStamp1")          = priceStamp1,
                                                            Rcpp::Named("priceStamp2")          = priceStamp2,
                                                            Rcpp::Named("priceStamp3")          = priceStamp3,
                                                            Rcpp::Named("priceStamp4")          = priceStamp4,
                                                            Rcpp::Named("priceStamp5")          = priceStamp5,
                                                            Rcpp::Named("priceStampBreakOut")   = priceStampBreakOut
   );
   
   Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
     Rcpp::Named("Rendite1V")     = Rendite1V,
     Rcpp::Named("Rendite3V")     = Rendite3V,
     Rcpp::Named("Rendite5V")     = Rendite5V,
     Rcpp::Named("Rendite10V")    = Rendite10V,
     Rcpp::Named("Rendite30V")    = Rendite30V,
     Rcpp::Named("Rendite60V")    = Rendite60V,
     Rcpp::Named("relRendite13V") = relRendite13V,
     Rcpp::Named("relRendite12V") = relRendite12V,
     Rcpp::Named("relRendite1V")  = relRendite1V,
     Rcpp::Named("relRendite2V")  = relRendite2V,
     Rcpp::Named("relRendite4V")  = relRendite4V
   );
   
  // Return a list containing all the data frames with pattern information
  return Rcpp::List::create(
    Rcpp::Named("patternInfo")     = patternInfo,
                             Rcpp::Named("Features2")       = Features2,
                             Rcpp::Named("Features21to40")  = Features21to41
   );
}

// Implementation of helper functions

// Efficient boundary checking
inline bool isValidIndex(int idx, int maxSize) {
  return (idx >= 0 && idx < maxSize);
}

// Generic pattern detection for both SHS and iSHS
bool detectPattern(const NumericVector& prices, const NumericVector& times, int i, 
                 bool isInverted, double& leftNecklineValue, double& rightNecklineValue, 
                 double& firstPointNecklineValue) {
  
  // Precalculate neckline values to avoid redundant calculations
  leftNecklineValue = linearInterpolation(times[i+2], times[i+4], 
                                        prices[i+2], prices[i+4], 
                                        times[i+1]);
  
  rightNecklineValue = linearInterpolation(times[i+2], times[i+4], 
                                         prices[i+2], prices[i+4], 
                                         times[i+5]);
                                         
  firstPointNecklineValue = linearInterpolation(times[i+2], times[i+4], 
                                              prices[i+2], prices[i+4], 
                                              times[i]);
  
  if (isInverted) {
    // iSHS pattern conditions
    return (prices[i] > prices[i+1] &&             // First point must be a high
            prices[i] > prices[i+2] &&             // First point higher than neckline start
            prices[i+1] > prices[i+3] &&           // Left shoulder higher than head
            prices[i+5] > prices[i+3] &&           // Right shoulder higher than head
            prices[i+5] < rightNecklineValue &&    // Right shoulder below neckline
            prices[i+1] < leftNecklineValue &&     // Left shoulder below neckline
            prices[i] > firstPointNecklineValue);  // First point above neckline
  } else {
    // SHS pattern conditions
    return (prices[i] < prices[i+1] &&             // First point must be a low
            prices[i] < prices[i+2] &&             // First point lower than neckline start
            prices[i+1] < prices[i+3] &&           // Left shoulder lower than head
            prices[i+5] < prices[i+3] &&           // Right shoulder lower than head
            prices[i+5] > rightNecklineValue &&    // Right shoulder above neckline
            prices[i+1] > leftNecklineValue &&     // Left shoulder above neckline
            prices[i] < firstPointNecklineValue);  // First point below neckline
  }
}

// Efficient return calculation
void calculateReturns(const NumericVector& prices, const NumericVector& times,
                    int breakoutIdx, int patternStartIdx, std::vector<double>& returns,
                    std::vector<double>& relReturns) {
  
  // Fixed time windows to check (1,3,5,10,30,60)
  const std::vector<int> fixedWindows = {1, 3, 5, 10, 30, 60};
  
  // Ensure returns vector has correct size
  if (returns.size() != fixedWindows.size()) {
    returns.resize(fixedWindows.size(), 0);
  }
  
  // Ensure relReturns vector has correct size (1/3, 1/2, 1, 2, 4)
  if (relReturns.size() != 5) {
    relReturns.resize(5, 0);
  }
  
  // Calculate pattern length for relative time windows
  int patternLengthInDays = times[breakoutIdx] - times[patternStartIdx];
  
  // Calculate relative time differences
  int relDiff13 = patternLengthInDays/3;
  int relDiff12 = patternLengthInDays/2;
  int relDiff1  = patternLengthInDays;
  int relDiff2  = patternLengthInDays*2;
  int relDiff4  = patternLengthInDays*4;
  
  // Store relative diffs in a vector for easier processing
  std::vector<int> relWindows = {relDiff13, relDiff12, relDiff1, relDiff2, relDiff4};
  
  // Initialize all return values to 0
  std::fill(returns.begin(), returns.end(), 0);
  std::fill(relReturns.begin(), relReturns.end(), 0);
  
  // Initialize flags to track which returns have been found
  std::vector<bool> foundFixed(fixedWindows.size(), false);
  std::vector<bool> foundRel(relWindows.size(), false);
  
  // Breakout price (used for calculating returns)
  double breakoutPrice = prices[breakoutIdx];
  
  // Loop through future data once, checking all time periods
  for(int forward = breakoutIdx + 1; forward < prices.size(); ++forward) {
    
    // Calculate time difference from breakout point
    int timeDiff = times[forward] - times[breakoutIdx];
    
    // Check each fixed window
    for(size_t w = 0; w < fixedWindows.size(); ++w) {
      if(!foundFixed[w] && timeDiff > fixedWindows[w]) {
        // Calculate return: for iSHS use log return, for SHS just price
        if (prices[breakoutIdx] > 0) {
          returns[w] = prices[forward];
        } else {
          returns[w] = log(prices[forward] / breakoutPrice);
        }
        foundFixed[w] = true;
      }
    }
    
    // Check each relative window
    for(size_t w = 0; w < relWindows.size(); ++w) {
      if(!foundRel[w] && timeDiff > relWindows[w]) {
        // For relative returns, always use price ratio
        relReturns[w] = prices[forward] / breakoutPrice;
        foundRel[w] = true;
      }
    }
    
    // Optimization: if we've found all returns, we can exit early
    bool allFound = std::all_of(foundFixed.begin(), foundFixed.end(), [](bool v){ return v; }) &&
                    std::all_of(foundRel.begin(), foundRel.end(), [](bool v){ return v; });
    
    if(allFound || timeDiff > MAX_LOOK_AHEAD) {
      break;
    }
  }
  
  // Set any returns we couldn't find to -1
  for(size_t w = 0; w < fixedWindows.size(); ++w) {
    if(!foundFixed[w]) {
      returns[w] = -1;
    }
  }
  
  for(size_t w = 0; w < relWindows.size(); ++w) {
    if(!foundRel[w]) {
      relReturns[w] = -1;
    }
  }
}

// Example of a simple pool allocator for pattern data
template <typename T>
class PoolAllocator {
    // Implementation of a memory pool for pattern data
};

// Use a more efficient approach for finding returns at different intervals
void calculateReturns(std::vector<double>& returns, 
                     const NumericVector& prices, 
                     const NumericVector& times,
                     int startIdx, int breakoutIdx,
                     const std::vector<int>& timeIntervals) {
    // More efficient return calculation
}

// Example of using SIMD with Eigen library
void calculateMultipleInterpolations(const VectorXd& x, const VectorXd& y,
                                    const VectorXd& queryPoints,
                                    VectorXd& results) {
    // Vectorized linear interpolation
 }

