#include "ISHSDetector.hpp"
#include <cmath>

/**
 * @file ISHSDetector.cpp
 * @brief Implements an Inverted Shoulder-Head-Shoulder (ISHS) pattern detector
 * 
 * The ISHS pattern is a bullish reversal pattern that forms after a downtrend.
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
 *      ___          ___
 *     /   \        /   \
 * ___/     \______/     \___
 *            \    /
 *             \  /
 *     Left     \/      Right
 *   Shoulder    Head   Shoulder
 *      \_/      \_/      \_/
 */

// Implementation of detect method
bool ISHSDetector::detect(const NumericVector& prices, const NumericVector& times, int position, PatternData& outPattern) const {
    
    // Need 6 points for a complete ISHS pattern (points 0-5)
    // 0: Starting point
    // 1: Left shoulder (trough)
    // 2: Peak between left shoulder and head
    // 3: Head (lowest trough)
    // 4: Peak between head and right shoulder
    // 5: Right shoulder (trough)
    if (position + 5 >= prices.size()) {
        return false;  // Not enough points remaining in the series
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
    
    // Check all ISHS pattern conditions
    bool isValid = 
        // 1. Basic price relationships - correct sequence of highs and lows
        prices[position] > prices[position+1] &&         // First point is a high (above left shoulder)
        prices[position] > prices[position+2] &&         // First point is higher than first peak
        prices[position+1] > prices[position+3] &&       // Left shoulder must be higher than head
        prices[position+5] > prices[position+3] &&       // Right shoulder must be higher than head
        
        // 2. Neckline conditions - shoulders must be below the neckline
        prices[position+5] < rightNecklineValue &&      // Right shoulder is below the neckline
        prices[position+1] < leftNecklineValue &&       // Left shoulder is below the neckline
        
        // First point check - pattern should start above the neckline
        prices[position] > firstPointNecklineValue;     // First point is above the neckline
        
    // If any condition is not met, this is not a valid ISHS pattern
    if (!isValid) {
        return false;
    }
    
    // Pattern is valid - initialize the pattern data structure
    outPattern.patternName = "iSHS";
    outPattern.startIdx = position;
    outPattern.detector = this;  // Store reference to this detector

    // Store key indices in the pattern
    outPattern.leftShoulderIdx = position + 1;
    outPattern.necklineStartIdx = position + 2;
    outPattern.headIdx = position + 3;
    outPattern.necklineEndIdx = position + 4;
    outPattern.rightShoulderIdx = position + 5;

    // Breakout hasn't been detected yet
    outPattern.breakoutIdx = NA_INTEGER;  
    
    // Store timestamps and price values for all points in the pattern
    // Double-check that vectors are properly sized before storing data
    if (outPattern.timeStamps.size() < 7) {
        outPattern.timeStamps.resize(7, 0);
    }
    if (outPattern.priceStamps.size() < 7) {
        outPattern.priceStamps.resize(7, 0.0);
    }
    
    // Store pattern points with bounds checking
    for (int i = 0; i < 6 && (position + i) < prices.size(); i++) {
        outPattern.timeStamps[i] = times[position + i];
        outPattern.priceStamps[i] = prices[position + i];
    }
    
    return true;  // Valid ISHS pattern detected
}

// Implementation of detectBreakout method
bool ISHSDetector::detectBreakout(const NumericVector& prices, const NumericVector& times, int currentIndexPosition, PatternData& pattern) const {
    
    // Breakout can only happen after the right shoulder and we need one more point to confirm
    // CRITICAL FIX: Safe bounds checking before accessing currentIndexPosition+1
    if (currentIndexPosition <= pattern.rightShoulderIdx || 
        currentIndexPosition >= prices.size() - 1) {  // Ensure there's one more position available
        return false;
    }
    
    // Safe access to timeStamps and priceStamps arrays
    if (pattern.timeStamps.size() < 5 || pattern.priceStamps.size() < 5) {
        return false; // Not enough data in pattern arrays
    }
    
    // Calculate the neckline value at the current position j
    double necklineValue = safeLinearInterpolation(
        pattern.timeStamps[2], // Left trough timestamp (index 2 in pattern array)
        pattern.timeStamps[4], // Right trough timestamp (index 4 in pattern array)
        pattern.priceStamps[2], // Left trough price
        pattern.priceStamps[4], // Right trough price
        times[currentIndexPosition]
    );
    
    // Check only this specific position for iSHS breakout:
    // 1. Price crosses above the neckline (bullish breakout)
    // 2. Next price remains above the right shoulder
    bool priceAboveNeckline = prices[currentIndexPosition] > necklineValue;
    
    // CRITICAL FIX: Use right shoulder price from pattern array instead of accessing original prices
    // This avoids out-of-bounds memory access that causes std::bad_alloc
    double rightShoulderPrice = pattern.priceStamps[5]; // Right shoulder is at index 5 in pattern array
    
    // CRITICAL FIX: We've already checked that currentIndexPosition+1 is in bounds above
    bool nextPriceAboveRightShoulder = prices[currentIndexPosition+1] > rightShoulderPrice;
    
    bool breakoutDetected = priceAboveNeckline && nextPriceAboveRightShoulder;
    
    // Store breakout data if a breakout is detected
    if (breakoutDetected) {
        // Store the breakout index (for reference)
        pattern.breakoutIdx = currentIndexPosition + 1; // +1 because we're checking the next price
        
        // Make sure there's enough space in the arrays
        if (pattern.timeStamps.size() > 6 && pattern.priceStamps.size() > 6) {
            // Store breakout time and price (at index 6)
            pattern.timeStamps[6] = times[currentIndexPosition + 1];
            pattern.priceStamps[6] = prices[currentIndexPosition + 1];
        }
    }
    
    return breakoutDetected;
}

// Implementation of getName method
std::string ISHSDetector::getName() const {
    return "iSHS";  // Return the pattern name
}

// Implementation of isPatternInvalidated method
bool ISHSDetector::isPatternInvalidated(const NumericVector& prices, const NumericVector& times, int position, PatternData& pattern) const {
    // For iSHS, invalidation occurs when price falls below the right shoulder
    // But NOT at the right shoulder position itself (special case)

    // Only check after the right shoulder is formed
    if (position > pattern.rightShoulderIdx) {
        // Bounds checking to avoid memory access issues
        if (position >= prices.size() || pattern.priceStamps.size() <= 5) {
            return false; // Cannot determine invalidation without sufficient data
        }
        
        // Get current price and right shoulder price for comparison
        double currentPrice = prices[position];
        double rightShoulderPrice = pattern.priceStamps[5]; // Right shoulder is at index 5 in pattern array
        
        // For iSHS: Invalidation when price falls BELOW right shoulder
        if (currentPrice < rightShoulderPrice) {
            return true;
        }
    }
    
    return false;  // Pattern remains valid
}

// Implementation of updateReturns method to avoid nested loops
bool ISHSDetector::updateReturns(const NumericVector& prices, const NumericVector& times,
                               int currentPosition, PatternData& pattern) const {
    // Skip if no breakout or current position is before/at breakout
    if (pattern.breakoutIdx == NA_INTEGER || currentPosition <= pattern.breakoutIdx) {
        return false;
    }
    
    // CRITICAL FIX: Validate current position is within bounds
    if (currentPosition >= prices.size() || currentPosition >= times.size()) {
        return false;
    }
    
    // CRITICAL FIX: We should store the breakout price in the pattern when breakout is detected,
    // rather than re-accessing the original array with a potentially large index
    
    // Check if breakout price has been stored in the pattern data (at index 6)
    double breakoutPrice;
    if (pattern.priceStamps.size() > 6 && pattern.priceStamps[6] != 0.0 && !R_IsNA(pattern.priceStamps[6])) {
        breakoutPrice = pattern.priceStamps[6];
    } else {
        // Fallback with bounds checking - still risky but better than before
        if (pattern.breakoutIdx >= 0 && pattern.breakoutIdx < prices.size()) {
            breakoutPrice = prices[pattern.breakoutIdx];
            // Store it for future use if we have space
            if (pattern.priceStamps.size() > 6) {
                pattern.priceStamps[6] = breakoutPrice;
            }
        } else {
            return false; // Invalid breakout index
        }
    }
    
    // Get breakout time with similar safety checks
    int breakoutTime;
    if (pattern.timeStamps.size() > 6 && pattern.timeStamps[6] != 0 && !R_IsNA(pattern.timeStamps[6])) {
        breakoutTime = pattern.timeStamps[6];
    } else {
        // Fallback with bounds checking
        if (pattern.breakoutIdx >= 0 && pattern.breakoutIdx < times.size()) {
            breakoutTime = times[pattern.breakoutIdx];
            // Store it for future use
            if (pattern.timeStamps.size() > 6) {
                pattern.timeStamps[6] = breakoutTime;
            }
        } else {
            return false; // Invalid breakout index
        }
    }
    
    // Calculate time difference from breakout point - using stored breakout time
    int timeDiff = times[currentPosition] - breakoutTime;
    
    // Fixed time windows to check (1,3,5,10,30,60 periods after breakout)
    const std::vector<int> fixedWindows = {1, 3, 5, 10, 30, 60};
    
    // Ensure returns vectors are properly sized
    if (pattern.returns.size() != fixedWindows.size()) {
        pattern.returns.resize(fixedWindows.size(), NA_REAL);
    }
    
    // Ensure relReturns vectors are properly sized (should be 5)
    if (pattern.relReturns.size() != 5) {
        pattern.relReturns.resize(5, NA_REAL);
    }
    
    // Calculate pattern length for relative time windows - use startIdx with bounds checking
    int patternStartTime;
    if (pattern.startIdx >= 0 && pattern.startIdx < times.size()) {
        patternStartTime = times[pattern.startIdx];
    } else {
        // Fallback to safe value from pattern data
        patternStartTime = pattern.timeStamps[0];
    }
    
    int patternLengthInDays = breakoutTime - patternStartTime;
    
    // CRITICAL FIX: Sanity check for pattern length to prevent invalid memory operations
    if (patternLengthInDays <= 0) {
        patternLengthInDays = 1; // Use minimum safe value to prevent division by zero
    }
    
    // Calculate relative time differences
    int relDiff13 = patternLengthInDays/3;  // 1/3 of pattern length
    int relDiff12 = patternLengthInDays/2;  // 1/2 of pattern length
    int relDiff1  = patternLengthInDays;    // Same as pattern length
    int relDiff2  = patternLengthInDays*2;  // Double pattern length
    int relDiff4  = patternLengthInDays*4;  // Quadruple pattern length
    
    // Store relative diffs in a vector for easier processing
    std::vector<int> relWindows = {relDiff13, relDiff12, relDiff1, relDiff2, relDiff4};
    
    // Initialize tracking arrays if this is the first call after breakout
    if (currentPosition == pattern.breakoutIdx + 1) {
        pattern.fixedWindowsFound.resize(fixedWindows.size(), false);
        pattern.relWindowsFound.resize(relWindows.size(), false);
    }
    
    // CRITICAL FIX: Validate tracking arrays are properly sized
    if (pattern.fixedWindowsFound.size() != fixedWindows.size()) {
        pattern.fixedWindowsFound.resize(fixedWindows.size(), false);
    }
    
    if (pattern.relWindowsFound.size() != relWindows.size()) {
        pattern.relWindowsFound.resize(relWindows.size(), false);
    }
    
    // Check fixed time windows
    for (size_t w = 0; w < fixedWindows.size(); ++w) {
        if (!pattern.fixedWindowsFound[w] && timeDiff > fixedWindows[w]) {
            // For ISHS, we expect higher prices after breakout (bullish pattern)
            // Use log return for 1-period, price ratio for others
            if (w == 0) {
                pattern.returns[w] = log(prices[currentPosition] / breakoutPrice);  // Log return for 1-period
            } else {
                pattern.returns[w] = prices[currentPosition] / breakoutPrice;  // Price ratio for other periods
            }
            pattern.fixedWindowsFound[w] = true;
        }
    }
    
    // Check relative time windows
    for (size_t w = 0; w < relWindows.size(); ++w) {
        if (!pattern.relWindowsFound[w] && timeDiff > relWindows[w]) {
            // For relative returns, always use price ratio
            pattern.relReturns[w] = prices[currentPosition] / breakoutPrice;
            pattern.relWindowsFound[w] = true;
        }
    }
    
    // Check if all returns have been found
    bool allFixedFound = std::all_of(pattern.fixedWindowsFound.begin(), pattern.fixedWindowsFound.end(), [](bool v) { return v; });
    bool allRelFound = std::all_of(pattern.relWindowsFound.begin(), pattern.relWindowsFound.end(), [](bool v) { return v; });
    
    // Return true if all returns have been calculated
    return allFixedFound && allRelFound;
}
