#include "SHSDetector.hpp"

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
 */

// Implementation of detect method
bool SHSDetector::detect(const NumericVector& prices, const NumericVector& times, int position, PatternData& outPattern) const {
    // Need 6 points for a complete SHS pattern (points 0-5)
    if (position + 5 >= prices.size()) {
        return false;  // Not enough points remaining in the series
    }
    
    // Calculate the neckline values at key positions
    double leftNecklineValue = safeLinearInterpolation(
        times[position+2], times[position+4],  // x-coordinates of the troughs
        prices[position+2], prices[position+4], // y-coordinates of the troughs 
        times[position+1]  // x-coordinate where we want the neckline value (left shoulder)
    );
    
    double rightNecklineValue = safeLinearInterpolation(
        times[position+2], times[position+4], 
        prices[position+2], prices[position+4], 
        times[position+5]  // x-coordinate where we want the neckline value (right shoulder)
    );
    
    double firstPointNecklineValue = safeLinearInterpolation(
        times[position+2], times[position+4], 
        prices[position+2], prices[position+4], 
        times[position]  // x-coordinate where we want the neckline value (first point)
    );
    
    // Check all SHS pattern conditions
    bool isValid = 
        // Basic price relationships - correct sequence of highs and lows
        prices[position] < prices[position+1] &&         // First point is a low (below left shoulder)
        prices[position] < prices[position+2] &&         // First point is lower than first trough
        prices[position+1] < prices[position+3] &&       // Left shoulder must be lower than head
        prices[position+5] < prices[position+3] &&       // Right shoulder must be lower than head
        
        // Neckline conditions - shoulders must be above the neckline
        prices[position+5] > rightNecklineValue &&      // Right shoulder is above the neckline
        prices[position+1] > leftNecklineValue &&       // Left shoulder is above the neckline
        
        // First point check - pattern should start below the neckline
        prices[position] < firstPointNecklineValue;     // First point is below the neckline
        
    // If any condition is not met, this is not a valid SHS pattern
    if (!isValid) {
        return false;
    }
    
    // Pattern is valid - initialize the pattern data structure
    outPattern.patternName = "SHS";
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
    
    return true;  // Valid SHS pattern detected
}

// Implementation of detectBreakout method - checks for breakout at a specific position
bool SHSDetector::detectBreakout(const NumericVector& prices, const NumericVector& times, int currentIndexPosition, PatternData& pattern) const {
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
    
    // Calculate the neckline value at the current position currentIndexPosition
    // Neckline is always interpolated between neckline points (not shoulders)
    double necklineValue = safeLinearInterpolation(
        pattern.timeStamps[2], // Left trough timestamp (index 2 in pattern array)
        pattern.timeStamps[4], // Right trough timestamp (index 4 in pattern array)
        pattern.priceStamps[2], // Left trough price
        pattern.priceStamps[4], // Right trough price
        times[currentIndexPosition]
    );
    
    // Check only this specific position for SHS breakout:
    // 1. Price crosses below the neckline (bearish breakout)
    // 2. Next price remains below the right shoulder
    bool priceBelowNeckline = prices[currentIndexPosition] < necklineValue;
    
    // CRITICAL FIX: Use right shoulder price from pattern array instead of accessing original prices
    // This avoids out-of-bounds memory access that causes std::bad_alloc
    double rightShoulderPrice = pattern.priceStamps[5]; // Right shoulder is at index 5 in pattern array
    
    // CRITICAL FIX: We've already checked that currentIndexPosition+1 is in bounds above
    bool nextPriceBelowRightShoulder = prices[currentIndexPosition+1] < rightShoulderPrice;
    
    bool breakoutDetected = priceBelowNeckline && nextPriceBelowRightShoulder;
    
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

// Implementation of pattern name getter
std::string SHSDetector::getName() const {
    return "SHS";
}

// Implementation of pattern invalidation checker - checks if pattern is invalidated at a specific position
bool SHSDetector::isPatternInvalidated(const NumericVector& prices, const NumericVector& times, int position, PatternData& pattern) const {
    // For SHS, invalidation occurs when price rises above the right shoulder
    // But NOT at the right shoulder position itself (special case)
    
    // Only check this specific position, not all positions after right shoulder
    if (position > pattern.rightShoulderIdx) {
        // Bounds checking to avoid memory access issues
        if (position >= prices.size() || pattern.priceStamps.size() <= 5) {
            return false; // Cannot determine invalidation without sufficient data
        }
        
        // Get current price and right shoulder price for comparison
        double currentPrice = prices[position];
        double rightShoulderPrice = pattern.priceStamps[5]; // Right shoulder is at index 5 in pattern array
        
        // For SHS: Invalidation when price rises ABOVE right shoulder
        if (currentPrice > rightShoulderPrice) {
            return true;
        }
    }
    
    return false;  // Pattern remains valid
}

// Implementation of updateReturns method to avoid nested loops
bool SHSDetector::updateReturns(const NumericVector& prices, const NumericVector& times,
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
    
    try {
        // Ensure returns vectors are properly sized
        if (pattern.returns.size() != fixedWindows.size()) {
            pattern.returns.resize(fixedWindows.size(), NA_REAL);
        }
        
        // Ensure relReturns vectors are properly sized (should be 5)
        if (pattern.relReturns.size() != 5) {
            pattern.relReturns.resize(5, NA_REAL);
        }
    } catch (const std::exception& e) {
        return false;
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
        try {
            pattern.fixedWindowsFound.resize(fixedWindows.size(), false);
            pattern.relWindowsFound.resize(relWindows.size(), false);
        } catch (const std::exception& e) {
            return false;
        }
    }
    
    // CRITICAL FIX: Validate tracking arrays are properly sized
    try {
        if (pattern.fixedWindowsFound.size() != fixedWindows.size()) {
            pattern.fixedWindowsFound.resize(fixedWindows.size(), false);
        }
        
        if (pattern.relWindowsFound.size() != relWindows.size()) {
            pattern.relWindowsFound.resize(relWindows.size(), false);
        }
    } catch (const std::exception& e) {
        return false;
    }
    
    // Check fixed time windows
    for (size_t w = 0; w < fixedWindows.size(); ++w) {
        if (!pattern.fixedWindowsFound[w] && timeDiff > fixedWindows[w]) {
            // For SHS, we expect lower prices after breakout (bearish pattern)
            // Use log return for 1-period, price ratio for others
            if (w == 0) {
                pattern.returns[w] = log(breakoutPrice / prices[currentPosition]);  // Log return for 1-period
            } else {
                pattern.returns[w] = breakoutPrice / prices[currentPosition];  // Price ratio for other periods
            }
            pattern.fixedWindowsFound[w] = true;
        }
    }
    
    // Check relative time windows
    for (size_t w = 0; w < relWindows.size(); ++w) {
        if (!pattern.relWindowsFound[w] && timeDiff > relWindows[w]) {
            // For relative returns, always use price ratio
            pattern.relReturns[w] = breakoutPrice / prices[currentPosition];
            pattern.relWindowsFound[w] = true;
        }
    }
    
    // Check if all returns have been found
    bool allFixedFound = std::all_of(pattern.fixedWindowsFound.begin(), pattern.fixedWindowsFound.end(), [](bool v) { return v; });
    bool allRelFound = std::all_of(pattern.relWindowsFound.begin(), pattern.relWindowsFound.end(), [](bool v) { return v; });
    
    // Return true if all returns have been calculated
    return allFixedFound && allRelFound;
}

