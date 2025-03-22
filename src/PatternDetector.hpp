#ifndef PATTERN_DETECTOR_HPP
#define PATTERN_DETECTOR_HPP

/**
 * @file PatternDetector.hpp
 * @brief Core interface for the chart pattern detection system
 * 
 * SYSTEM OVERVIEW
 * ---------------
 * This pattern detection system implements an extensible, object-oriented framework
 * for identifying and analyzing chart patterns in financial time series data.
 * 
 * The system consists of the following key components:
 * 
 * 1. PatternDetector Interface (this file):
 *    - Defines the common interface that all pattern detectors must implement
 *    - Provides default implementations for common functionality (e.g., return calculations)
 *    - Defines data structures for storing pattern information
 * 
 * 2. Pattern-Specific Detectors (SHSDetector.cpp, ISHSDetector.cpp):
 *    - Implement the PatternDetector interface for specific patterns
 *    - Contain the pattern-specific detection, breakout, and trend logic
 *    - Currently supported patterns: Shoulder-Head-Shoulder (SHS) and inverse SHS (iSHS)
 * 
 * 3. Main Function (findPatterns.cpp):
 *    - Serves as the entry point for pattern detection
 *    - Manages detector lifecycle and orchestrates the pattern detection process
 *    - Handles data preprocessing and result formatting for R
 * 
 * 4. Utility Functions (safeLinearInterpolation.cpp):
 *    - Provides common mathematical functions used across detectors
 *    - Implements safe versions of operations to prevent numerical issues
 * 
 * DATA FLOW
 * ---------
 * 1. Input data (prices and timestamps) is passed to the system
 * 2. findPatterns creates detector instances for each requested pattern
 * 3. Each detector scans the data to identify potential patterns
 * 4. When a pattern is found, it's monitored for a breakout
 * 5. Upon breakout, performance metrics and trend information are calculated
 * 6. Results are formatted and returned to the caller
 * 
 * EXTENDING THE SYSTEM
 * -------------------
 * To add a new pattern detector:
 * 1. Create a new class that implements the PatternDetector interface
 * 2. Implement the required methods (detect, detectBreakout, getName)
 * 3. Add the new detector to the detector creation in findPatterns.cpp
 * 
 * PERFORMANCE CONSIDERATIONS
 * -------------------------
 * - Pattern detection uses early rejection conditions to minimize computation
 * - Memory pre-allocation is used to prevent frequent reallocations
 * - Pattern count is dynamically calculated based on input data size
 * - Return calculation optimizations prevent redundant passes through the data
 */

#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// --------------------------------------------------------------------------
// Global Constants - Used across pattern detection implementations
// --------------------------------------------------------------------------
// Note: Pattern count is calculated dynamically based on input data size.
// We don't need a hardcoded constant for this anymore.
const int INVALID_TIME = 99999991;       // Sentinel value used to indicate an invalid or unset time value

// --------------------------------------------------------------------------
// Forward Declarations - Helper functions used by pattern detectors
// --------------------------------------------------------------------------
// Calculates a y-value at a given x position using linear interpolation between two points
// with division-by-zero protection
double safeLinearInterpolation(double x1, double x2, double y1, double y2, double atPosition);
// Calculates the slope between two points (used for various feature calculations)
double getSlope(double x1, double x2, double y1, double y2);

// --------------------------------------------------------------------------
// Pattern Data Structure - Holds all information about a detected pattern
// --------------------------------------------------------------------------
struct PatternData {
    // Key indices marking important points in the pattern
    int startIdx;                   // First point of pattern
    int leftShoulderIdx;            // Left shoulder index
    int necklineStartIdx;           // Start of neckline index
    int headIdx;                    // Head index (highest/lowest point)
    int necklineEndIdx;             // End of neckline index
    int rightShoulderIdx;           // Right shoulder index
    int breakoutIdx;                // Breakout point index (when price crosses the neckline)
    
    std::string patternName;        // Name of the pattern ("SHS" or "iSHS")
    std::vector<int> timeStamps;    // Time points of all key positions in the pattern
    std::vector<double> priceStamps; // Price points of all key positions in the pattern
    
    // Trend information (calculated after pattern detection)
    double trendBeginPrice;         // Price at beginning of trend before pattern
    int trendBeginTime;             // Time at beginning of trend before pattern
    double trendEndPrice;           // Price at end of trend after pattern
    int trendEndTime;               // Time at end of trend after pattern
    
    // Performance metrics (calculated after breakout)
    std::vector<double> returns;    // Returns at fixed time windows (1,3,5,10,30,60 periods)
    std::vector<double> relReturns; // Returns at relative time windows (1/3, 1/2, 1, 2, 4 times pattern length)
    
    // Default constructor - Initialize all fields with default values
    PatternData() : 
        startIdx(-1), leftShoulderIdx(-1), necklineStartIdx(-1), 
        headIdx(-1), necklineEndIdx(-1), rightShoulderIdx(-1), breakoutIdx(-1),
        patternName(""), trendBeginPrice(-1.0), trendBeginTime(INVALID_TIME),
        trendEndPrice(-1.0), trendEndTime(INVALID_TIME) {}
};

// --------------------------------------------------------------------------
// Pattern Detector Interface - Base class for all pattern detectors
// --------------------------------------------------------------------------
class PatternDetector {
public:
    // Virtual destructor for proper inheritance
    virtual ~PatternDetector() = default;
    
    // Detect a pattern starting at position in the time series
    // This is the primary detection method that checks if a pattern exists
    // Returns true if a valid pattern is found, with details in outPattern
    virtual bool detect(const NumericVector& prices, const NumericVector& times, 
                       int position, PatternData& outPattern) const = 0;
    
    // Checks if a breakout of the pattern has occurred at the given position
    // For SHS: breakout is when price crosses below the neckline
    // For iSHS: breakout is when price crosses above the neckline
    virtual bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                              int position, const PatternData& pattern) const = 0;
    
    // Checks if a pattern has been invalidated by price action at the given position
    // Different patterns have different invalidation criteria
    // Returns true if the pattern should no longer be considered valid
    virtual bool isPatternInvalidated(const NumericVector& prices, const NumericVector& times,
                                   int position, const PatternData& pattern) const = 0;
    
    // Returns the name of this pattern type (e.g., "SHS" or "iSHS")
    virtual std::string getName() const = 0;
    
    // Calculates trend information before and after the pattern
    // This helps analyze the context in which the pattern formed
    virtual void calculateTrend(const NumericVector& prices, const NumericVector& times,
                              const PatternData& pattern, int breakoutIdx,
                              double& trendBeginPrice, int& trendBeginTime,
                              double& trendEndPrice, int& trendEndTime) const {
        // Default implementation - can be overridden by specific detectors
        trendBeginPrice = -1.0;
        trendBeginTime = INVALID_TIME;
        trendEndPrice = -1.0;
        trendEndTime = INVALID_TIME;
    }
    
    // Calculates return metrics after a pattern breakout
    // This helps evaluate the performance/profitability of trading based on the pattern
    virtual void calculateReturns(const NumericVector& prices, const NumericVector& times,
                               int breakoutIdx, int patternStartIdx, 
                               std::vector<double>& returns,
                               std::vector<double>& relReturns) const {
        // Validate inputs
        if (breakoutIdx < 0 || breakoutIdx >= prices.size()) {
            Rcpp::warning("Invalid breakout index in calculateReturns");
            std::fill(returns.begin(), returns.end(), NA_REAL);
            std::fill(relReturns.begin(), relReturns.end(), NA_REAL);
            return;
        }
        
        if (patternStartIdx < 0 || patternStartIdx >= prices.size()) {
            Rcpp::warning("Invalid pattern start index in calculateReturns");
            std::fill(returns.begin(), returns.end(), NA_REAL);
            std::fill(relReturns.begin(), relReturns.end(), NA_REAL);
            return;
        }
        
        if (R_IsNA(prices[breakoutIdx]) || R_IsNaN(prices[breakoutIdx])) {
            Rcpp::warning("Invalid breakout price (NA/NaN) in calculateReturns");
            std::fill(returns.begin(), returns.end(), NA_REAL);
            std::fill(relReturns.begin(), relReturns.end(), NA_REAL);
            return;
        }
        
        // Fixed time windows to check (1,3,5,10,30,60 periods after breakout)
        const std::vector<int> fixedWindows = {1, 3, 5, 10, 30, 60};
        
        // Ensure vectors are the correct size
        if (returns.size() != fixedWindows.size()) {
            returns.resize(fixedWindows.size(), 0);
        }
        
        // Relative time windows based on pattern length
        if (relReturns.size() != 5) {
            relReturns.resize(5, 0);
        }
        
        // Calculate pattern length to determine relative time windows
        int patternLengthInDays = times[breakoutIdx] - times[patternStartIdx];
        
        // Handle patterns with zero or negative length (shouldn't happen, but being safe)
        if (patternLengthInDays <= 0) {
            Rcpp::warning("Pattern has zero or negative length in days");
            patternLengthInDays = 1; // Use a default minimum length
        }
        
        // Calculate relative time differences (1/3, 1/2, 1, 2, 4 times pattern length)
        int relDiff13 = patternLengthInDays/3;
        int relDiff12 = patternLengthInDays/2;
        int relDiff1  = patternLengthInDays;
        int relDiff2  = patternLengthInDays*2;
        int relDiff4  = patternLengthInDays*4;
        
        std::vector<int> relWindows = {relDiff13, relDiff12, relDiff1, relDiff2, relDiff4};
        
        // Initialize all return values to NA (R's missing value)
        std::fill(returns.begin(), returns.end(), NA_REAL);
        std::fill(relReturns.begin(), relReturns.end(), NA_REAL);
        
        // Tracking which returns have been found
        std::vector<bool> foundFixed(fixedWindows.size(), false);
        std::vector<bool> foundRel(relWindows.size(), false);
        
        // Breakout price is our reference for calculating returns
        double breakoutPrice = prices[breakoutIdx];
        
        // Scan through future data to find returns at specific time windows
        for (int forward = breakoutIdx + 1; forward < prices.size(); ++forward) {
            
            // Skip invalid data points
            if (R_IsNA(prices[forward]) || R_IsNaN(prices[forward]) || 
                R_IsNA(times[forward]) || R_IsNaN(times[forward])) {
                continue;
            }
            
            // How many periods have passed since breakout
            int timeDiff = times[forward] - times[breakoutIdx];
            
            // Check if we've reached each fixed time window
            for (size_t w = 0; w < fixedWindows.size(); ++w) {
                if (!foundFixed[w] && timeDiff > fixedWindows[w]) {
                    // Save the price at this time window
                    returns[w] = prices[forward];
                    foundFixed[w] = true;
                }
            }
            
            // Check if we've reached each relative time window
            for (size_t w = 0; w < relWindows.size(); ++w) {
                if (!foundRel[w] && timeDiff > relWindows[w]) {
                    // For relative returns, calculate price ratio
                    relReturns[w] = prices[forward] / breakoutPrice;
                    foundRel[w] = true;
                }
            }
            
            // Exit early only if all returns have been found
            bool allFound = std::all_of(foundFixed.begin(), foundFixed.end(), [](bool v){ return v; }) &&
                           std::all_of(foundRel.begin(), foundRel.end(), [](bool v){ return v; });
            
            // Exit if all returns found, otherwise continue to end of data
            if (allFound) {
                break;
            }
        }
    }
};

// The inline implementation of safeLinearInterpolation is removed.
// We'll use the implementation from safeLinearInterpolation.cpp instead.

#endif // PATTERN_DETECTOR_HPP 