#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

// Enable OpenMP if available
#ifdef _OPENMP
  #include <omp.h>
#endif

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(Rcpp)]]

using namespace Rcpp;

// Optimized linear interpolation - inlined for speed
inline double linearInterp(double x1, double x2, double y1, double y2, double x) {
  return y1 + (y2 - y1) * (x - x1) / (x2 - x1);
}

// Optimized slope calculation - inlined for speed
inline double getSlope(double x1, double x2, double y1, double y2) {
  return (y2 - y1) / (x2 - x1);
}

// Result structure for pattern detection
struct PatternResult {
  int firstIdxPrePro;       // First pattern point index in pivot series
  int firstIdxOrig;         // First pattern point index in original series
  int breakoutIdx;          // Breakout index in original series (or NA if invalid)
  int timeStamp;            // Breakout timestamp
  int priceStamp;           // Breakout price
  int patternLengthDays;    // Pattern duration in time units
  bool isSHS;               // True for SHS, false for iSHS
  bool isValid;             // True if pattern has valid breakout

  // Feature metrics for the pattern
  double slopeNeckline;
  double slopePIP[7];       // Slopes between consecutive points
  double length[7];         // Lengths between consecutive points
  double lengthNeckline;

  // Trend metrics before and after pattern
  int risingLowsBefore;     // Rising lows count before
  int risingHighsBefore;    // Rising highs count before
  int fallingLowsBefore;    // Falling lows count before
  int fallingHighsBefore;   // Falling highs count before
  int fallingHighsAfter;    // Falling highs count after
  int risingLowsAfter;      // Rising lows count after
};

//' @name UltraFastFind
//' @title Ultra Optimized Pattern Detection: SHS and iSHS
//' @description High-performance implementation for detecting Shoulder-Head-Shoulder (SHS) and 
//'              inverted Shoulder-Head-Shoulder (iSHS) patterns using data-oriented design and 
//'              single-pass processing.
//' @param PrePro_indexFilter Vector of indices identifying pivot points in the original data series
//' @param Original_times Vector with timestamps or indices for each data point
//' @param Original_prices Vector with price values corresponding to each timestamp
//' @return DataFrame with pattern information and metrics
//' @details
//'   This implementation preserves the exact same detection logic as the original
//'   but runs significantly faster through optimization techniques:
//'   - Data-oriented design with contiguous memory
//'   - Single-pass algorithm
//'   - Precomputation of shared values
//'   - No heap allocations in critical path
//' @examples
//' c(1:10)
//'
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame UltraFastFind(IntegerVector PrePro_indexFilter,
                              NumericVector Original_times,
                              NumericVector Original_prices) {
  
  // Validate inputs - fail fast
  if (PrePro_indexFilter.size() < 7 || Original_times.size() < 2 || Original_prices.size() < 2) {
    return Rcpp::DataFrame::create();
  }
  
  // Verify index vector validity
  for (int i = 0; i < PrePro_indexFilter.size(); ++i) {
    if (PrePro_indexFilter[i] < 0 || PrePro_indexFilter[i] >= Original_prices.size()) {
      return Rcpp::DataFrame::create(); 
    }
  }
  
  // Extract pivot points - create contiguous arrays for faster access
  const int M = PrePro_indexFilter.size();
  const int N = Original_prices.size();
  
  // Pre-extract pivot data for contiguous access
  std::vector<double> QuerySeries_times(M);
  std::vector<double> QuerySeries_prices(M);
  
  for (int i = 0; i < M; ++i) {
    QuerySeries_times[i] = Original_times[PrePro_indexFilter[i]];
    QuerySeries_prices[i] = Original_prices[PrePro_indexFilter[i]];
  }
  
  // Get raw pointers for fast access
  const int* idx = PrePro_indexFilter.begin();
  const double* T = QuerySeries_times.data();
  const double* P = QuerySeries_prices.data();
  const double* FT = Original_times.begin();
  const double* FP = Original_prices.begin();
  
  // Result container - pre-allocate based on realistic estimate
  std::vector<PatternResult> results;
  results.reserve(M / 6);  // Heuristic: at most 1 pattern per 6 pivots
  
  // Main pattern detection loop - vectorizable with proper compiler flags
  #pragma omp parallel for if(M > 1000) schedule(dynamic)
  for (int i = 0; i + 5 < M; ++i) {
    // Pre-fetch the 6 pivots into local variables for faster access
    const double t0 = T[i], p0 = P[i];
    const double t1 = T[i+1], p1 = P[i+1];
    const double t2 = T[i+2], p2 = P[i+2];
    const double t3 = T[i+3], p3 = P[i+3];
    const double t4 = T[i+4], p4 = P[i+4];
    const double t5 = T[i+5], p5 = P[i+5];
    
    // Compute neckline parameters once - reuse for all checks
    const double slope = getSlope(t2, t4, p2, p4);
    const double intercept = p2 - slope * t2;
    
    // Compute neckline values at important points
    const double n0 = linearInterp(t2, t4, p2, p4, t0);  // Neckline at first point
    const double n1 = linearInterp(t2, t4, p2, p4, t1);  // Neckline at left shoulder
    const double n5 = linearInterp(t2, t4, p2, p4, t5);  // Neckline at right shoulder
    
    // Check if SHS pattern
    bool isSHS = p0 < p1 && p0 < p2 &&      // First point below left shoulder and trough
                 p1 < p3 && p5 < p3 &&      // Shoulders below head
                 p5 > n5 && p1 > n1 &&      // Shoulders above neckline
                 p0 < n0;                   // First point below neckline
    
    // Check if iSHS pattern
    bool isISHS = p0 > p1 && p0 > p2 &&     // First point above left shoulder and peak
                  p1 > p3 && p5 > p3 &&     // Shoulders above head
                  p5 < n5 && p1 < n1 &&     // Shoulders below neckline
                  p0 > n0;                  // First point above neckline
    
    // Skip if not a pattern
    if (!isSHS && !isISHS) continue;
    
    // Local result to build before thread-safe insertion
    PatternResult result;
    result.firstIdxPrePro = i + 1;
    result.firstIdxOrig = idx[i] + 1;
    result.isSHS = isSHS;
    result.isValid = false;
    result.breakoutIdx = NA_INTEGER;
    
    // Compute pattern features
    result.slopeNeckline = slope;
    
    // Store slopes between pivot points
    result.slopePIP[0] = getSlope(t0, t1, p0, p1);
    result.slopePIP[1] = getSlope(t1, t2, p1, p2);
    result.slopePIP[2] = getSlope(t2, t3, p2, p3);
    result.slopePIP[3] = getSlope(t3, t4, p3, p4);
    result.slopePIP[4] = getSlope(t4, t5, p4, p5);
    
    // Store lengths between pivot points
    result.length[0] = t1 - t0;
    result.length[1] = t2 - t1;
    result.length[2] = t3 - t2;
    result.length[3] = t4 - t3;
    result.length[4] = t5 - t4;
    result.lengthNeckline = t2 - t5;
    
    // Default placeholder values for non-valid patterns
    result.slopePIP[5] = 0;
    result.slopePIP[6] = 0;
    result.length[5] = 0;
    result.length[6] = 0;
    
    // Initialize trends to zero (will compute only if pattern is valid)
    result.risingLowsBefore = 0;
    result.risingHighsBefore = 0;
    result.fallingLowsBefore = 0;
    result.fallingHighsBefore = 0;
    result.fallingHighsAfter = 0;
    result.risingLowsAfter = 0;
    
    result.timeStamp = t0;
    result.priceStamp = p0;
    result.patternLengthDays = 0;
    
    // Check for breakout
    if (i + 5 < M && idx[i + 5] < N - 1) {
      int j0 = idx[i + 5] + 1;
      
      for (int j = j0; j < N - 1; ++j) {
        // Compute neckline at current position
        double necklineValue = slope * FT[j] + intercept;
        
        // Pattern invalidation checks
        if ((isSHS && FP[j] > p5 && j != j0) || 
            (isISHS && FP[j] < p5 && j != j0)) {
          break;
        }
        
        // Breakout detection
        if ((isSHS && FP[j] < necklineValue && FP[j+1] < p5) || 
            (isISHS && FP[j] > necklineValue && FP[j+1] > p5)) {
          // Valid breakout found
          result.isValid = true;
          result.breakoutIdx = j + 1;
          result.timeStamp = FT[j+1];
          result.priceStamp = FP[j+1];
          result.patternLengthDays = FT[j+1] - t0;
          
          // Calculate t6 if we have enough points
          if (i + 6 < M) {
            const double t6 = T[i+6];
            const double p6 = P[i+6];
            result.slopePIP[5] = getSlope(t5, t6, p5, p6);
            result.length[5] = t6 - t5;
            
            // Add slope to breakout
            result.slopePIP[6] = getSlope(t5, FT[j], p5, FP[j]);
            result.length[6] = FT[j] - t6;
          }
          
          // Compute trend metrics for valid patterns
          // Rising lows before (every 2nd point)
          for (int rev = i; rev > 1; rev -= 2) {
            if (P[rev] > P[rev-2]) {
              result.risingLowsBefore++;
            } else {
              break;
            }
          }
          
          // Rising highs before (every 2nd point)
          for (int rev = i-1; rev > 1; rev -= 2) {
            if (P[rev] > P[rev-2]) {
              result.risingHighsBefore++;
            } else {
              break;
            }
          }
          
          // Falling lows before
          for (int rev = i; rev > 1; rev -= 2) {
            if (P[rev] < P[rev-2]) {
              result.fallingLowsBefore++;
            } else {
              break;
            }
          }
          
          // Falling highs before
          for (int rev = i-1; rev > 1; rev -= 2) {
            if (P[rev] < P[rev-2]) {
              result.fallingHighsBefore++;
            } else {
              break;
            }
          }
          
          // Falling highs after
          for (int fwd = i+4; fwd+2 < M; fwd += 2) {
            if (P[fwd] > P[fwd+2]) {
              result.fallingHighsAfter++;
            } else {
              break;
            }
          }
          
          // Rising lows after
          for (int fwd = i+4; fwd+2 < M; fwd += 2) {
            if (P[fwd] < P[fwd+2]) {
              result.risingLowsAfter++;
            } else {
              break;
            }
          }
          
          break;  // Exit the breakout loop
        }
      }
    }
    
    // Thread-safe insertion of result
    #pragma omp critical
    {
      results.push_back(result);
    }
  }
  
  // Convert results to R data frames
  
  // Extract all data from results vector to separate vectors for DataFrame
  std::vector<std::string> patternName;
  std::vector<std::string> patternGroup;
  std::vector<bool> validPattern;
  std::vector<int> patternLength;
  std::vector<int> firstIndexPrePro;
  std::vector<int> firstIndexOrigi;
  std::vector<int> breakoutIndex;
  std::vector<int> timeStamp;
  std::vector<int> priceStamp;
  std::vector<int> patternLengthInDays;
  
  // Pattern feature vectors
  std::vector<double> slopeNeckline;
  std::vector<double> slopePIP_1;
  std::vector<double> slopePIP_2;
  std::vector<double> slopePIP_3;
  std::vector<double> slopePIP_4;
  std::vector<double> slopePIP_5;
  std::vector<double> slopePIP_6;
  std::vector<double> slopePIP_7;
  std::vector<double> length_1;
  std::vector<double> length_2;
  std::vector<double> length_3;
  std::vector<double> length_4;
  std::vector<double> length_5;
  std::vector<double> length_6;
  std::vector<double> length_7;
  std::vector<double> length_Neckline;
  
  // Trend vectors
  std::vector<int> AnzahlAufsteigenderTiefpunkteBefore; // Rising lows before
  std::vector<int> AnzahlAufsteigenderHochpunkteBefore; // Rising highs before
  std::vector<int> AnzahlAbsteigenderTiefpunkteBefore;  // Falling lows before
  std::vector<int> AnzahlAbsteigenderHochpunkteBefore;  // Falling highs before
  std::vector<int> AnzahlAbsteigenderHochpunkteAfter;   // Falling highs after
  std::vector<int> AnzahlAufsteigenderTiefpunkteAfter;  // Rising lows after
  
  // Pre-allocate vectors to avoid reallocations
  const size_t numResults = results.size();
  patternName.reserve(numResults);
  patternGroup.reserve(numResults);
  validPattern.reserve(numResults);
  patternLength.reserve(numResults);
  firstIndexPrePro.reserve(numResults);
  firstIndexOrigi.reserve(numResults);
  breakoutIndex.reserve(numResults);
  timeStamp.reserve(numResults);
  priceStamp.reserve(numResults);
  patternLengthInDays.reserve(numResults);
  
  slopeNeckline.reserve(numResults);
  slopePIP_1.reserve(numResults);
  slopePIP_2.reserve(numResults);
  slopePIP_3.reserve(numResults);
  slopePIP_4.reserve(numResults);
  slopePIP_5.reserve(numResults);
  slopePIP_6.reserve(numResults);
  slopePIP_7.reserve(numResults);
  length_1.reserve(numResults);
  length_2.reserve(numResults);
  length_3.reserve(numResults);
  length_4.reserve(numResults);
  length_5.reserve(numResults);
  length_6.reserve(numResults);
  length_7.reserve(numResults);
  length_Neckline.reserve(numResults);
  
  AnzahlAufsteigenderTiefpunkteBefore.reserve(numResults);
  AnzahlAufsteigenderHochpunkteBefore.reserve(numResults);
  AnzahlAbsteigenderTiefpunkteBefore.reserve(numResults);
  AnzahlAbsteigenderHochpunkteBefore.reserve(numResults);
  AnzahlAbsteigenderHochpunkteAfter.reserve(numResults);
  AnzahlAufsteigenderTiefpunkteAfter.reserve(numResults);
  
  // Copy data from results to vectors
  for (const auto& result : results) {
    patternName.push_back(result.isSHS ? "SHS" : "iSHS");
    patternGroup.push_back("SHS");
    validPattern.push_back(result.isValid);
    patternLength.push_back(5);
    firstIndexPrePro.push_back(result.firstIdxPrePro);
    firstIndexOrigi.push_back(result.firstIdxOrig);
    breakoutIndex.push_back(result.breakoutIdx);
    timeStamp.push_back(result.timeStamp);
    priceStamp.push_back(result.priceStamp);
    patternLengthInDays.push_back(result.patternLengthDays);
    
    slopeNeckline.push_back(result.slopeNeckline);
    slopePIP_1.push_back(result.slopePIP[0]);
    slopePIP_2.push_back(result.slopePIP[1]);
    slopePIP_3.push_back(result.slopePIP[2]);
    slopePIP_4.push_back(result.slopePIP[3]);
    slopePIP_5.push_back(result.slopePIP[4]);
    slopePIP_6.push_back(result.slopePIP[5]);
    slopePIP_7.push_back(result.slopePIP[6]);
    length_1.push_back(result.length[0]);
    length_2.push_back(result.length[1]);
    length_3.push_back(result.length[2]);
    length_4.push_back(result.length[3]);
    length_5.push_back(result.length[4]);
    length_6.push_back(result.length[5]);
    length_7.push_back(result.length[6]);
    length_Neckline.push_back(result.lengthNeckline);
    
    AnzahlAufsteigenderTiefpunkteBefore.push_back(result.risingLowsBefore);
    AnzahlAufsteigenderHochpunkteBefore.push_back(result.risingHighsBefore);
    AnzahlAbsteigenderTiefpunkteBefore.push_back(result.fallingLowsBefore);
    AnzahlAbsteigenderHochpunkteBefore.push_back(result.fallingHighsBefore);
    AnzahlAbsteigenderHochpunkteAfter.push_back(result.fallingHighsAfter);
    AnzahlAufsteigenderTiefpunkteAfter.push_back(result.risingLowsAfter);
  }
  
  // Create the DataFrames with the same structure as in FastFindII
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
    Rcpp::Named("PatternName")              = patternName,
    Rcpp::Named("PatternGroup")             = patternGroup,
    Rcpp::Named("validPattern")             = validPattern,
    Rcpp::Named("patternLength")            = patternLength,
    Rcpp::Named("firstIndexinPrePro")       = firstIndexPrePro,
    Rcpp::Named("firstIndexinOriginal")     = firstIndexOrigi,
    Rcpp::Named("breakoutIndexinOrig")      = breakoutIndex,
    Rcpp::Named("TimeStamp")                = timeStamp,
    Rcpp::Named("PriceStamp")               = priceStamp,
    Rcpp::Named("patternLengthInDays")      = patternLengthInDays
  );
  
  // Shape features of detected patterns
  Rcpp::DataFrame Features1to20 = Rcpp::DataFrame::create(
    Rcpp::Named("f01")  = slopePIP_1,
    Rcpp::Named("f02")  = slopePIP_2,
    Rcpp::Named("f03")  = slopePIP_3,
    Rcpp::Named("f04")  = slopePIP_4,
    Rcpp::Named("f05")  = slopePIP_5,
    Rcpp::Named("f06")  = slopePIP_6,
    Rcpp::Named("f07")  = slopePIP_7,
    Rcpp::Named("f08")  = length_1,
    Rcpp::Named("f09")  = length_2,
    Rcpp::Named("f10")  = length_3,
    Rcpp::Named("f11")  = length_4,
    Rcpp::Named("f12")  = length_5,
    Rcpp::Named("f13")  = length_6,
    Rcpp::Named("f14")  = length_7,
    Rcpp::Named("slopeNackenlinie")  = slopeNeckline,
    Rcpp::Named("lengthNackenlinie") = length_Neckline
  );
  
  // Trend metrics around the pattern
  Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
    Rcpp::Named("AnzahlAufsteigenderTiefpunkteBefore") = AnzahlAufsteigenderTiefpunkteBefore,
    Rcpp::Named("AnzahlAufsteigenderHochpunkteBefore") = AnzahlAufsteigenderHochpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderTiefpunkteBefore")  = AnzahlAbsteigenderTiefpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderHochpunkteBefore")  = AnzahlAbsteigenderHochpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderHochpunkteAfter")   = AnzahlAbsteigenderHochpunkteAfter,
    Rcpp::Named("AnzahlAufsteigenderTiefpunkteAfter")  = AnzahlAufsteigenderTiefpunkteAfter
  );
  
  // Return the same list structure as FastFindII
  return Rcpp::List::create(
    Rcpp::Named("patternInfo")     = patternInfo,
    Rcpp::Named("Features1to20")   = Features1to20,
    Rcpp::Named("Features21to40")  = Features21to41
  );
} 