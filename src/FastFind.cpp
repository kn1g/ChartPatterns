#include <vector>
#include <string>
#include <cmath>
#include "cppHeader.hpp"
#include <RcppEigen.h>
#include <algorithm>
#include <Rcpp.h>

/**
 * @file FastFind.cpp
 * @brief Implements fast pattern recognition for financial chart patterns
 * 
 * This file provides implementation for detecting chart patterns in financial time series data.
 * Currently supports Shoulder-Head-Shoulder (SHS) and inverse Shoulder-Head-Shoulder (iSHS) patterns.
 * The implementation uses preprocessed pivot points to efficiently identify potential patterns.
 */

// Forward declarations
bool isValidIndex(int idx, int maxSize);

// Define the PatternData struct before using it
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
        // Simple placeholder implementation - replace with actual logic
        bool patternDetected = false;
        
        // Check for SHS pattern conditions
        if (position + 5 < prices.size() &&
            prices[position] < prices[position+1] && 
            prices[position+1] < prices[position+3] && 
            prices[position+5] < prices[position+3]) {
            
            // Initialize pattern data
            outPattern.patternName = "SHS";
            outPattern.startIdx = position;
            outPattern.leftShoulderIdx = position + 1;
            outPattern.necklineStartIdx = position + 2;
            outPattern.headIdx = position + 3;
            outPattern.necklineEndIdx = position + 4;
            outPattern.rightShoulderIdx = position + 5;
            outPattern.breakoutIdx = position + 6; // Placeholder
            
            // Set timestamps and price stamps
            outPattern.timeStamps.resize(7);
            outPattern.priceStamps.resize(7);
            for (int i = 0; i < 6; i++) {
                outPattern.timeStamps[i] = times[position + i];
                outPattern.priceStamps[i] = prices[position + i];
            }
            // Last position is for breakout (placeholder)
            if (position + 6 < times.size()) {
                outPattern.timeStamps[6] = times[position + 6];
                outPattern.priceStamps[6] = prices[position + 6];
            }
            
            patternDetected = true;
        }
        
        return patternDetected;
    }
    
    bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                       int j, const PatternData& pattern) const override {
        // SHS-specific breakout detection
        // Simple placeholder implementation - replace with actual logic
        bool breakoutDetected = false;
        
        if (j > pattern.rightShoulderIdx && j < prices.size()) {
            // Implement actual breakout detection logic here
            breakoutDetected = true;
        }
        
        return breakoutDetected;
    }
    
    std::string getName() const override {
        return "SHS";
    }
};

class ISHSDetector : public PatternDetector {
public:
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override {
        // ISHS-specific detection logic
        // Simple placeholder implementation - replace with actual logic
        bool patternDetected = false;
        
        // Check for ISHS pattern conditions
        if (position + 5 < prices.size() &&
            prices[position] > prices[position+1] && 
            prices[position+1] > prices[position+3] && 
            prices[position+5] > prices[position+3]) {
            
            // Initialize pattern data
            outPattern.patternName = "iSHS";
            outPattern.startIdx = position;
            outPattern.leftShoulderIdx = position + 1;
            outPattern.necklineStartIdx = position + 2;
            outPattern.headIdx = position + 3;
            outPattern.necklineEndIdx = position + 4;
            outPattern.rightShoulderIdx = position + 5;
            outPattern.breakoutIdx = position + 6; // Placeholder
            
            // Set timestamps and price stamps
            outPattern.timeStamps.resize(7);
            outPattern.priceStamps.resize(7);
            for (int i = 0; i < 6; i++) {
                outPattern.timeStamps[i] = times[position + i];
                outPattern.priceStamps[i] = prices[position + i];
            }
            // Last position is for breakout (placeholder)
            if (position + 6 < times.size()) {
                outPattern.timeStamps[6] = times[position + 6];
                outPattern.priceStamps[6] = prices[position + 6];
            }
            
            patternDetected = true;
        }
        
        return patternDetected;
    }
    
    bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                       int j, const PatternData& pattern) const override {
        // ISHS-specific breakout detection
        // Simple placeholder implementation - replace with actual logic
        bool breakoutDetected = false;
        
        if (j > pattern.rightShoulderIdx && j < prices.size()) {
            // Implement actual breakout detection logic here
            breakoutDetected = true;
        }
        
        return breakoutDetected;
    }
    
    std::string getName() const override {
        return "iSHS";
    }
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
   
  // Use the global PatternData struct - Remove the duplicate definition here
  // Create a struct to hold pattern data
  
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
    // Note: These values are used in pattern detection but may not be needed here
    
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
    }
  }
  
  // Extract data from patterns for returning
  std::vector<std::string> patternNames;
  std::vector<int> startIndices;
  std::vector<int> leftShoulderIndices;
  std::vector<int> headIndices;
  std::vector<int> rightShoulderIndices;

  // Loop through the detected patterns and populate the vectors
  for (const auto& pattern : patterns) {
    patternNames.push_back(pattern.patternName);
    startIndices.push_back(pattern.startIdx);
    leftShoulderIndices.push_back(pattern.leftShoulderIdx);
    headIndices.push_back(pattern.headIdx);
    rightShoulderIndices.push_back(pattern.rightShoulderIdx);
  }

  // Create a simplified dataframe with just essential information for testing
  Rcpp::DataFrame result = Rcpp::DataFrame::create(
    Rcpp::Named("PatternName") = patternNames,
    Rcpp::Named("startIdx") = startIndices,
    Rcpp::Named("leftShoulderIdx") = leftShoulderIndices,
    Rcpp::Named("headIdx") = headIndices,
    Rcpp::Named("rightShoulderIdx") = rightShoulderIndices
  );

  return result;
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
void calculateMultipleInterpolations(const Eigen::VectorXd& x, const Eigen::VectorXd& y,
                                    const Eigen::VectorXd& queryPoints,
                                    Eigen::VectorXd& results) {
    // Vectorized linear interpolation
 }

