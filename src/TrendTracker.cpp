#include "TrendTracker.hpp"

/**
 * @file TrendTracker.cpp
 * @brief Implementation of the TrendTracker class for global trend tracking
 */

// Constructor - initializes all trend counters and indices
TrendTracker::TrendTracker() {
    reset();
}

// Reset all trend counters and indices
void TrendTracker::reset() {
    // Reset counters
    ascHighCount = 0;
    ascLowCount = 0;
    descHighCount = 0;
    descLowCount = 0;
    
    // Reset previous counters
    prevAscHighCount = 0;
    prevAscLowCount = 0;
    prevDescHighCount = 0;
    prevDescLowCount = 0;
    
    // Reset indices
    ascHighFirstIdx = -1;
    ascLowFirstIdx = -1;
    descHighFirstIdx = -1;
    descLowFirstIdx = -1;
    
    // Reset prices
    ascHighFirstPrice = -1.0;
    ascLowFirstPrice = -1.0;
    descHighFirstPrice = -1.0;
    descLowFirstPrice = -1.0;
    
    // Reset times
    ascHighFirstTime = -1;
    ascLowFirstTime = -1;
    descHighFirstTime = -1;
    descLowFirstTime = -1;
}

// Update trend counters based on current price data
// Returns true if any trend was reset, false otherwise
bool TrendTracker::updateTrends(const Rcpp::NumericVector& prices, const Rcpp::NumericVector& times, int position) {
    // Need at least 3 points to detect a trend
    if (position < 2) {
        return false;
    }
    
    // Store previous counts to detect resets
    prevAscHighCount = ascHighCount;
    prevAscLowCount = ascLowCount;
    prevDescHighCount = descHighCount;
    prevDescLowCount = descLowCount;
    
    bool trendReset = false;
    
    // Instead of checking if a point is high or low based on position,
    // directly check consecutive points of the same type (every 2 positions)
    
    // Check if current position and position-2 are the same type (both highs or both lows)
    // We don't care which one - we just look at price relationships
    
    // Case 1: Current point is higher than previous point of same type
    if (prices[position] > prices[position-2]) {
        // For even positions (lows) or odd positions (highs)
        if (!isPivotHigh(position)) {
            if (ascLowCount == 0) {
                ascLowFirstIdx = position-2;
                ascLowFirstPrice = prices[position-2];
                ascLowFirstTime = times[position-2];
            }
            ascLowCount++;
            
            // Check if descending low trend was reset
            if (descLowCount > 0) {
                descLowCount = 0;
                trendReset = true;
            }
        } 
        else {
            if (ascHighCount == 0) {
                ascHighFirstIdx = position-2;
                ascHighFirstPrice = prices[position-2];
                ascHighFirstTime = times[position-2];
            }
            ascHighCount++;
            
            // Check if descending high trend was reset
            if (descHighCount > 0) {
                descHighCount = 0;
                trendReset = true;
            }
        }
    }
    // Case 2: Current point is lower than previous point of same type
    else if (prices[position] < prices[position-2]) {
        // For even positions (lows) or odd positions (highs)
        if (!isPivotHigh(position)) {
            if (descLowCount == 0) {
                descLowFirstIdx = position-2;
                descLowFirstPrice = prices[position-2];
                descLowFirstTime = times[position-2];
            }
            descLowCount++;
            
            // Check if ascending low trend was reset
            if (ascLowCount > 0) {
                ascLowCount = 0;
                trendReset = true;
            }
        } 
        else {
            if (descHighCount == 0) {
                descHighFirstIdx = position-2;
                descHighFirstPrice = prices[position-2];
                descHighFirstTime = times[position-2];
            }
            descHighCount++;
            
            // Check if ascending high trend was reset
            if (ascHighCount > 0) {
                ascHighCount = 0;
                trendReset = true;
            }
        }
    }
    // If prices are equal, no changes to trends
    
    return trendReset;
}

// Apply initial trend information to a pattern based on its type
void TrendTracker::applyTrendInfo(PatternData& pattern) {
    if (pattern.patternName == "SHS") {
        // For SHS: prior trend should be ascending (uptrend before bearish reversal)
        // We care primarily about ascending lows, but also track ascending highs
        if (ascLowCount > 0) {
            pattern.setPriorTrendStartPrice(ascLowFirstPrice);
            pattern.setPriorTrendStartTime(ascLowFirstTime);
            pattern.setPriorTrendInfo(ascLowCount);
        } else {
            pattern.setPriorTrendStartPrice(-1.0);
            pattern.setPriorTrendStartTime(INVALID_TIME);
            pattern.setPriorTrendInfo(0);
        }
        
        // Mark prior trend as complete immediately - we already have this info
        pattern.markPriorTrendComplete();
        
        // Following trend not set yet (will be set when descending trend forms)
        pattern.followingTrendComplete = false;
    } 
    else if (pattern.patternName == "iSHS") {
        // For iSHS: prior trend should be descending (downtrend before bullish reversal)
        // We care primarily about descending highs, but also track descending lows
        if (descHighCount > 0) {
            pattern.setPriorTrendStartPrice(descHighFirstPrice);
            pattern.setPriorTrendStartTime(descHighFirstTime);
            pattern.setPriorTrendInfo(descHighCount);
        } else {
            pattern.setPriorTrendStartPrice(-1.0);
            pattern.setPriorTrendStartTime(INVALID_TIME);
            pattern.setPriorTrendInfo(0);
        }
        
        // Mark prior trend as complete immediately - we already have this info
        pattern.markPriorTrendComplete();
        
        // Following trend not set yet (will be set when ascending trend forms)
        pattern.followingTrendComplete = false;
    }
}

// Apply trend information to all patterns in the list that need updating
void TrendTracker::applyTrendInfoToPatterns(std::deque<std::unique_ptr<PatternData>>& patterns) {
    for (auto& patternPtr : patterns) {
        auto& pattern = *patternPtr;
        
        // Skip patterns that have been fully processed
        if (pattern.processed) continue;
        
        // Skip prior trend update section - prior trends are now applied at detection time
        
        // Update following trend information for SHS patterns with breakouts
        if (pattern.patternName == "SHS" && pattern.breakoutIdx != NA_INTEGER && !pattern.followingTrendComplete) {
            // For SHS, following trend should be descending (bearish after breakout)
            // Check if we have a descending trend started
            if (descLowCount > 0 || descHighCount > 0) {
                // Use whichever descending trend has more points
                if (descLowCount >= descHighCount) {
                    pattern.setFollowingTrendStartPrice(descLowFirstPrice);
                    pattern.setFollowingTrendStartTime(descLowFirstTime);
                    pattern.setFollowingTrendInfo(descLowCount);
                } else {
                    pattern.setFollowingTrendStartPrice(descHighFirstPrice);
                    pattern.setFollowingTrendStartTime(descHighFirstTime);
                    pattern.setFollowingTrendInfo(descHighCount);
                }
                
                // If we've descended significantly, mark following trend complete
                if (descLowCount >= 3 || descHighCount >= 3) {
                    pattern.markFollowingTrendComplete();
                }
            }
        }
        // Update following trend information for iSHS patterns with breakouts
        else if (pattern.patternName == "iSHS" && pattern.breakoutIdx != NA_INTEGER && !pattern.followingTrendComplete) {
            // For iSHS, following trend should be ascending (bullish after breakout)
            // Check if we have an ascending trend started
            if (ascLowCount > 0 || ascHighCount > 0) {
                // Use whichever ascending trend has more points
                if (ascLowCount >= ascHighCount) {
                    pattern.setFollowingTrendStartPrice(ascLowFirstPrice);
                    pattern.setFollowingTrendStartTime(ascLowFirstTime);
                    pattern.setFollowingTrendInfo(ascLowCount);
                } else {
                    pattern.setFollowingTrendStartPrice(ascHighFirstPrice);
                    pattern.setFollowingTrendStartTime(ascHighFirstTime);
                    pattern.setFollowingTrendInfo(ascHighCount);
                }
                
                // If we've ascended significantly, mark following trend complete
                if (ascLowCount >= 3 || ascHighCount >= 3) {
                    pattern.markFollowingTrendComplete();
                }
            }
        }
    }
}

// Apply final trend information at the end of processing
void TrendTracker::applyFinalTrendInfo(std::deque<std::unique_ptr<PatternData>>& patterns) {
    for (auto& patternPtr : patterns) {
        auto& pattern = *patternPtr;
        
        // Skip patterns that have been fully processed
        if (pattern.processed) continue;
        
        // For SHS patterns with breakouts, use whatever descending trend we have
        if (pattern.patternName == "SHS" && pattern.breakoutIdx != NA_INTEGER && !pattern.followingTrendComplete) {
            // For SHS, following trend should be descending (bearish after breakout)
            if (descLowCount > 0 || descHighCount > 0) {
                // Use whichever descending trend has more points
                if (descLowCount >= descHighCount) {
                    pattern.setFollowingTrendStartPrice(descLowFirstPrice);
                    pattern.setFollowingTrendStartTime(descLowFirstTime);
                    pattern.setFollowingTrendInfo(descLowCount);
                } else {
                    pattern.setFollowingTrendStartPrice(descHighFirstPrice);
                    pattern.setFollowingTrendStartTime(descHighFirstTime);
                    pattern.setFollowingTrendInfo(descHighCount);
                }
            }
            
            // Force completion since we're at the end of processing
            pattern.markFollowingTrendComplete();
        }
        // For iSHS patterns with breakouts, use whatever ascending trend we have
        else if (pattern.patternName == "iSHS" && pattern.breakoutIdx != NA_INTEGER && !pattern.followingTrendComplete) {
            // For iSHS, following trend should be ascending (bullish after breakout)
            if (ascLowCount > 0 || ascHighCount > 0) {
                // Use whichever ascending trend has more points
                if (ascLowCount >= ascHighCount) {
                    pattern.setFollowingTrendStartPrice(ascLowFirstPrice);
                    pattern.setFollowingTrendStartTime(ascLowFirstTime);
                    pattern.setFollowingTrendInfo(ascLowCount);
                } else {
                    pattern.setFollowingTrendStartPrice(ascHighFirstPrice);
                    pattern.setFollowingTrendStartTime(ascHighFirstTime);
                    pattern.setFollowingTrendInfo(ascHighCount);
                }
            }
            
            // Force completion since we're at the end of processing
            pattern.markFollowingTrendComplete();
        }
    }
} 