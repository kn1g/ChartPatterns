#include "PatternDetector.hpp"
#include <cmath>

/**
 * @file ISHSDetector.cpp
 * @brief Implements an Inverse Shoulder-Head-Shoulder (iSHS) pattern detector
 * 
 * The iSHS pattern is a bullish reversal pattern that forms after a downtrend.
 * It consists of:
 * 1. A trough (left shoulder)
 * 2. A lower trough (head)
 * 3. A higher trough (right shoulder)
 * 4. A neckline connecting the peaks between the troughs
 * 
 * The pattern is confirmed when price breaks above the neckline.
 * This implementation uses the exact conditions from the original code.
 * 
 * Visual representation:
 *             Neckline  Breakout
 *           ____________|  |
 *          /            \  /
 *         /              \/
 *    /\  /                \  /\
 *   /  \/                  \/  \
 *  /                            \
 * /       Head                   \
 *         /\                      
 *        /  \     Right Shoulder  
 * Left  /    \    /\             
 * Shoulder    \  /  \            
 *    /\        \/    \           
 *   /  \              \          
 */

class ISHSDetector : public PatternDetector {
public:
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override {
        
        // Need 6 points for a complete iSHS pattern (points 0-5)
        // 0: Starting point
        // 1: Left shoulder (trough)
        // 2: Peak between left shoulder and head
        // 3: Head (lowest trough)
        // 4: Peak between head and right shoulder
        // 5: Right shoulder (trough)
        if (position + 5 >= prices.size()) {
            return false;  // Not enough points remaining in the series
        }
        
        // Input validation - check for missing or invalid values
        for (int i = 0; i < 6; i++) {
            if (R_IsNA(prices[position + i]) || R_IsNA(times[position + i]) || 
                R_IsNaN(prices[position + i]) || R_IsNaN(times[position + i])) {
                return false;  // Invalid input data (contains NA or NaN)
            }
        }
        
        // Calculate the neckline as a straight line connecting the peaks
        // (points 2 and 4) and extending in both directions
        
        // Calculate neckline value at the left shoulder position
        double leftNecklineValue = safeLinearInterpolation(
            times[position+2], times[position+4],  // x-coordinates of the peaks
            prices[position+2], prices[position+4], // y-coordinates of the peaks
            times[position+1]  // x-coordinate where we want the neckline value (left shoulder)
        );
        
        // Calculate neckline value at the right shoulder position
        double rightNecklineValue = safeLinearInterpolation(
            times[position+2], times[position+4], 
            prices[position+2], prices[position+4], 
            times[position+5]  // x-coordinate where we want the neckline value (right shoulder)
        );
        
        // Calculate neckline value at the first point position
        double firstPointNecklineValue = safeLinearInterpolation(
            times[position+2], times[position+4], 
            prices[position+2], prices[position+4], 
            times[position]  // x-coordinate where we want the neckline value (first point)
        );
        
        // Check all iSHS pattern conditions according to the original implementation
        bool isValid = 
            // 1. Basic price relationships - correct sequence of highs and lows
            prices[position] > prices[position+1] &&         // First point is a high (above left shoulder)
            prices[position] > prices[position+2] &&         // First point is higher than first peak
            prices[position+1] > prices[position+3] &&       // Left shoulder must be higher than head
            prices[position+5] > prices[position+3] &&       // Right shoulder must be higher than head
            
            // 2. Neckline conditions - shoulders must be below the neckline
            prices[position+5] < rightNecklineValue &&      // Right shoulder is below the neckline
            prices[position+1] < leftNecklineValue &&       // Left shoulder is below the neckline
            
            // 3. First point check - pattern should start above the neckline
            // This ensures pattern isn't too skewed and starts from a proper position
            prices[position] > firstPointNecklineValue;     // First point is above the neckline
            
        // If any condition is not met, this is not a valid iSHS pattern
        if (!isValid) {
            return false;
        }
        
        // Pattern is valid - initialize the pattern data structure
        outPattern.patternName = "iSHS";
        outPattern.startIdx = position;
        outPattern.leftShoulderIdx = position + 1;
        outPattern.necklineStartIdx = position + 2;
        outPattern.headIdx = position + 3;
        outPattern.necklineEndIdx = position + 4;
        outPattern.rightShoulderIdx = position + 5;
        outPattern.breakoutIdx = -1;  // Breakout hasn't been detected yet
        
        // Store timestamps and price values for all points in the pattern
        outPattern.timeStamps.resize(7);  // 6 points + 1 for breakout (to be filled later)
        outPattern.priceStamps.resize(7);
        for (int i = 0; i < 6; i++) {
            outPattern.timeStamps[i] = times[position + i];
            outPattern.priceStamps[i] = prices[position + i];
        }
        
        return true;  // Valid iSHS pattern detected
    }
    
    bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                        int j, const PatternData& pattern) const override {
        
        // Breakout can only happen after the right shoulder and we need one more point to confirm
        if (j <= pattern.rightShoulderIdx || j >= prices.size() - 1) {
            return false;
        }
        
        // Calculate the neckline value at the current position j
        double necklineValue = safeLinearInterpolation(
            pattern.timeStamps[pattern.necklineStartIdx - pattern.startIdx], 
            pattern.timeStamps[pattern.necklineEndIdx - pattern.startIdx],
            pattern.priceStamps[pattern.necklineStartIdx - pattern.startIdx], 
            pattern.priceStamps[pattern.necklineEndIdx - pattern.startIdx],
            times[j]
        );
        
        // For iSHS, a breakout occurs when:
        // 1. Price crosses above the neckline (bullish breakout)
        // 2. Next price remains above the right shoulder (confirms the breakout)
        return (prices[j] > necklineValue && 
                prices[j+1] > pattern.priceStamps[pattern.rightShoulderIdx - pattern.startIdx]);
    }
    
    std::string getName() const override {
        return "iSHS";  // Return the pattern name
    }
    
    bool isPatternInvalidated(const NumericVector& prices, const NumericVector& times,
                            int position, const PatternData& pattern) const override {
        // For iSHS patterns, invalidation occurs when:
        // Price moves below the right shoulder after pattern formation
        // This invalidates the bullish sentiment of the pattern
        
        // Exception: don't check if we're at the right shoulder point itself
        if (position == pattern.rightShoulderIdx) {
            return false;
        }
        
        // Pattern is invalidated if price goes below the right shoulder
        double rightShoulderPrice = pattern.priceStamps[pattern.rightShoulderIdx - pattern.startIdx];
        return prices[position] < rightShoulderPrice;
    }
    
    void calculateTrend(const NumericVector& prices, const NumericVector& times,
                       const PatternData& pattern, int breakoutIdx,
                       double& trendBeginPrice, int& trendBeginTime,
                       double& trendEndPrice, int& trendEndTime) const override {
        
        // For iSHS patterns, we look backwards for descending high points
        // to identify the preceding downtrend (iSHS forms at the end of a downtrend)
        trendBeginPrice = -1.0;  // Default value if no trend is found
        trendBeginTime = INVALID_TIME;
        
        // Walk backwards in steps of 2 (looking at every other point)
        // Since pivot points alternate between highs and lows, stepping by 2
        // examines points of the same type (i.e., all highs or all lows)
        for (int rev = pattern.startIdx; rev > 2; rev -= 2) {
            if (prices[rev] < prices[rev-2]) {
                // Found a prior high that's lower than the one before it
                // This indicates a descending high point pattern, confirming downtrend
                trendBeginPrice = prices[rev-2];  // Store the earliest point in the trend
                trendBeginTime = times[rev-2];
            } else {
                // Once we find a high that's not lower than previous, the trend ends
                break;
            }
        }
        
        // For iSHS, we look forward for ascending low points after the pattern
        // to identify the following uptrend (iSHS initiates an uptrend)
        trendEndPrice = -1.0;  // Default value if no trend is found
        trendEndTime = INVALID_TIME;
        
        // Walk forward in steps of 2, looking at ascending lows
        for (int forward = pattern.rightShoulderIdx; forward < (prices.size() - 2); forward += 2) {
            if (prices[forward] < prices[forward+2]) {
                // Found a subsequent low that's higher than the previous one
                // This indicates an ascending low point pattern, confirming uptrend
                trendEndPrice = prices[forward+2];  // Store the furthest point in the trend
                trendEndTime = times[forward+2];
            } else {
                // Once we find a low that's not higher than the next, the trend ends
                break;
            }
        }
    }
    
    void calculateReturns(const NumericVector& prices, const NumericVector& times,
                         int breakoutIdx, int patternStartIdx, 
                         std::vector<double>& returns,
                         std::vector<double>& relReturns) const override {
        // For iSHS patterns, use specialized returns calculation
        // This is tailored specifically for iSHS patterns, with log returns for short timeframes
        
        // Fixed time windows to check (1,3,5,10,30,60 periods after breakout)
        const std::vector<int> fixedWindows = {1, 3, 5, 10, 30, 60};
        
        // Ensure returns vector has correct size
        if (returns.size() != fixedWindows.size()) {
            returns.resize(fixedWindows.size(), 0);
        }
        
        // Ensure relReturns vector has correct size (1/3, 1/2, 1, 2, 4 times pattern length)
        if (relReturns.size() != 5) {
            relReturns.resize(5, 0);
        }
        
        // Calculate pattern length for relative time windows
        int patternLengthInDays = times[breakoutIdx] - times[patternStartIdx];
        
        // Calculate relative time differences
        int relDiff13 = patternLengthInDays/3;  // 1/3 of pattern length
        int relDiff12 = patternLengthInDays/2;  // 1/2 of pattern length
        int relDiff1  = patternLengthInDays;    // Same as pattern length
        int relDiff2  = patternLengthInDays*2;  // Double pattern length
        int relDiff4  = patternLengthInDays*4;  // Quadruple pattern length
        
        // Store relative diffs in a vector for easier processing
        std::vector<int> relWindows = {relDiff13, relDiff12, relDiff1, relDiff2, relDiff4};
        
        // Initialize all return values to NA (R's missing value)
        std::fill(returns.begin(), returns.end(), NA_REAL);
        std::fill(relReturns.begin(), relReturns.end(), NA_REAL);
        
        // Initialize flags to track which returns have been found
        std::vector<bool> foundFixed(fixedWindows.size(), false);
        std::vector<bool> foundRel(relWindows.size(), false);
        
        // Breakout price is our reference for calculating returns
        double breakoutPrice = prices[breakoutIdx];
        
        // Loop through future data once, checking all time periods
        // Continue until all returns are found or we reach the end of the data
        for (int forward = breakoutIdx + 1; forward < prices.size(); ++forward) {
            
            // Calculate time difference from breakout point
            int timeDiff = times[forward] - times[breakoutIdx];
            
            // Check each fixed window
            for (size_t w = 0; w < fixedWindows.size(); ++w) {
                if (!foundFixed[w] && timeDiff > fixedWindows[w]) {
                    // For iSHS, use log return for first window, price ratios for others
                    // This matches the specific implementation in FastFind_ChaosRegin.cpp
                    if (w == 0) {
                        returns[w] = log(prices[forward] / breakoutPrice);  // Log return for 1-period
                    } else {
                        returns[w] = prices[forward] / breakoutPrice;  // Price ratio for other periods
                    }
                    foundFixed[w] = true;
                }
            }
            
            // Check each relative window
            for (size_t w = 0; w < relWindows.size(); ++w) {
                if (!foundRel[w] && timeDiff > relWindows[w]) {
                    // For relative returns, always use price ratio
                    relReturns[w] = prices[forward] / breakoutPrice;
                    foundRel[w] = true;
                }
            }
            
            // Optimization: if we've found all returns, we can exit early
            bool allFound = std::all_of(foundFixed.begin(), foundFixed.end(), [](bool v){ return v; }) &&
                           std::all_of(relReturns.begin(), relReturns.end(), [](bool v){ return v; });
            
            if (allFound) {
                break;  // Exit if all returns found
            }
        }
    }
}; 