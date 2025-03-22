#include "PatternDetector.hpp"

/**
 * @file SHSDetector.cpp
 * @brief Implements a Shoulder-Head-Shoulder (SHS) pattern detector
 * 
 * The SHS pattern is a bearish reversal pattern that forms after an uptrend.
 * It consists of:
 * 1. A peak (left shoulder)
 * 2. A higher peak (head)
 * 3. A lower peak (right shoulder)
 * 4. A neckline connecting the troughs between the peaks
 * 
 * The pattern is confirmed when price breaks below the neckline.
 * This implementation uses the exact conditions from the original code.
 * 
 * Visual representation:
 *                Head
 *                 /\
 *                /  \
 *     Left      /    \     Right
 *   Shoulder   /      \   Shoulder
 *      /\     /        \    /\
 *     /  \   /          \  /  \
 * ___/    \_/            \/    \___
 *            \          /
 *             \________/
 *                |  |
 *            Neckline  Breakout
 */

class SHSDetector : public PatternDetector {
public:
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override {
        
        // Need 6 points for a complete SHS pattern (points 0-5)
        // 0: Starting point
        // 1: Left shoulder (peak)
        // 2: Trough between left shoulder and head
        // 3: Head (highest peak)
        // 4: Trough between head and right shoulder
        // 5: Right shoulder (peak)
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
        
        // Calculate the neckline as a straight line connecting the troughs
        // (points 2 and 4) and extending in both directions
        
        // Calculate neckline value at the left shoulder position
        double leftNecklineValue = safeLinearInterpolation(
            times[position+2], times[position+4],  // x-coordinates of the troughs
            prices[position+2], prices[position+4], // y-coordinates of the troughs 
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
        
        // Check all SHS pattern conditions according to the original implementation
        bool isValid = 
            // 1. Basic price relationships - correct sequence of highs and lows
            prices[position] < prices[position+1] &&         // First point is a low (below left shoulder)
            prices[position] < prices[position+2] &&         // First point is lower than first trough
            prices[position+1] < prices[position+3] &&       // Left shoulder must be lower than head
            prices[position+5] < prices[position+3] &&       // Right shoulder must be lower than head
            
            // 2. Neckline conditions - shoulders must be above the neckline
            prices[position+5] > rightNecklineValue &&      // Right shoulder is above the neckline
            prices[position+1] > leftNecklineValue &&       // Left shoulder is above the neckline
            
            // 3. First point check - pattern should start below the neckline
            // This ensures pattern isn't too skewed and starts from a proper position
            prices[position] < firstPointNecklineValue;     // First point is below the neckline
            
        // If any condition is not met, this is not a valid SHS pattern
        if (!isValid) {
            return false;
        }
        
        // Pattern is valid - initialize the pattern data structure
        outPattern.patternName = "SHS";
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
        
        return true;  // Valid SHS pattern detected
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
        
        // For SHS, a breakout occurs when:
        // 1. Price crosses below the neckline (bearish breakout)
        // 2. Next price remains below the right shoulder (confirms the breakout)
        return (prices[j] < necklineValue && 
                prices[j+1] < pattern.priceStamps[pattern.rightShoulderIdx - pattern.startIdx]);
    }
    
    std::string getName() const override {
        return "SHS";  // Return the pattern name
    }
    
    bool isPatternInvalidated(const NumericVector& prices, const NumericVector& times,
                            int position, const PatternData& pattern) const override {
        // For SHS patterns, invalidation occurs when:
        // Price moves above the right shoulder after pattern formation
        // This invalidates the bearish sentiment of the pattern
        
        // Exception: don't check if we're at the right shoulder point itself
        if (position == pattern.rightShoulderIdx) {
            return false;
        }
        
        // Pattern is invalidated if price goes above the right shoulder
        double rightShoulderPrice = pattern.priceStamps[pattern.rightShoulderIdx - pattern.startIdx];
        return prices[position] > rightShoulderPrice;
    }
    
    void calculateTrend(const NumericVector& prices, const NumericVector& times,
                       const PatternData& pattern, int breakoutIdx,
                       double& trendBeginPrice, int& trendBeginTime,
                       double& trendEndPrice, int& trendEndTime) const override {
        
        // For SHS patterns, we look backwards for ascending low points
        // to identify the preceding uptrend (SHS forms at the end of an uptrend)
        trendBeginPrice = -1.0;  // Default value if no trend is found
        trendBeginTime = INVALID_TIME;
        
        // Walk backwards in steps of 2 (looking at every other point)
        // Since pivot points alternate between highs and lows, stepping by 2
        // examines points of the same type (i.e., all lows or all highs)
        for (int rev = pattern.startIdx; rev > 2; rev -= 2) {
            if (prices[rev] > prices[rev-2]) {
                // Found a prior low that's higher than the one before it
                // This indicates an ascending low point pattern, confirming uptrend
                trendBeginPrice = prices[rev-2];  // Store the earliest point in the trend
                trendBeginTime = times[rev-2];
            } else {
                // Once we find a low that's not higher than previous, the trend ends
                break;
            }
        }
        
        // For SHS, we look forward for descending high points after the pattern
        // to identify the following downtrend (SHS initiates a downtrend)
        trendEndPrice = -1.0;  // Default value if no trend is found
        trendEndTime = INVALID_TIME;
        
        // Walk forward in steps of 2, looking at descending highs
        for (int forward = pattern.rightShoulderIdx; forward < (prices.size() - 2); forward += 2) {
            if (prices[forward] > prices[forward+2]) {
                // Found a subsequent high that's lower than the previous one
                // This indicates a descending high point pattern, confirming downtrend
                trendEndPrice = prices[forward+2];  // Store the furthest point in the trend
                trendEndTime = times[forward+2];
            } else {
                // Once we find a high that's not lower than the next, the trend ends
                break;
            }
        }
    }
}; 