#ifndef TREND_TRACKER_HPP
#define TREND_TRACKER_HPP

#include <Rcpp.h>
#include "PatternDetector.hpp"
#include <vector>
#include <deque>
#include <memory>

/**
 * @file TrendTracker.hpp
 * @brief Header for the TrendTracker class, which manages trend tracking globally
 * 
 * This class tracks ascending and descending trends in price data.
 * It maintains counts and starting points for each of the four trend types:
 * 1. Ascending highs
 * 2. Ascending lows
 * 3. Descending highs
 * 4. Descending lows
 * 
 * This approach centralizes trend tracking logic rather than having it in individual detectors.
 */

class TrendTracker {
public:
    /**
     * @brief Default constructor - initializes all trend counters and indices
     */
    TrendTracker();
    
    /**
     * @brief Update trend counters based on current price data
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Current position in the data
     * @return bool Returns true if any trend was reset, false otherwise
     */
    bool updateTrends(const Rcpp::NumericVector& prices, const Rcpp::NumericVector& times, int position);
    
    /**
     * @brief Update a pattern with appropriate trend information based on pattern type
     * 
     * @param pattern Pattern to update with trend information
     */
    void applyTrendInfo(PatternData& pattern);
    
    /**
     * @brief Apply the current trend info to a list of patterns
     * 
     * @param patterns Vector of patterns to update
     */
    void applyTrendInfoToPatterns(std::deque<std::unique_ptr<PatternData>>& patterns);
    
    /**
     * @brief Apply final trend info at the end of processing
     * 
     * @param patterns Vector of patterns to update with final trend info
     */
    void applyFinalTrendInfo(std::deque<std::unique_ptr<PatternData>>& patterns);
    
    /**
     * @brief Reset all trend counters and indices
     */
    void reset();
    
private:
    // Trend counters
    int ascHighCount;    // Count of consecutive ascending highs
    int ascLowCount;     // Count of consecutive ascending lows
    int descHighCount;   // Count of consecutive descending highs
    int descLowCount;    // Count of consecutive descending lows
    
    // First index in each trend sequence
    int ascHighFirstIdx;
    int ascLowFirstIdx;
    int descHighFirstIdx;
    int descLowFirstIdx;
    
    // First price in each trend sequence
    double ascHighFirstPrice;
    double ascLowFirstPrice;
    double descHighFirstPrice;
    double descLowFirstPrice;
    
    // First timestamp in each trend sequence
    int ascHighFirstTime;
    int ascLowFirstTime;
    int descHighFirstTime;
    int descLowFirstTime;
    
    // Previous trend counters - used to detect resets
    int prevAscHighCount;
    int prevAscLowCount;
    int prevDescHighCount;
    int prevDescLowCount;
    
    // Helper function to determine if a pivot point is a high or low
    // In a zigzag PIP series, even indices are lows and odd indices are highs
    bool isPivotHigh(int position) const {
        return (position % 2 != 0); // Odd positions are highs
    }
};

#endif // TREND_TRACKER_HPP 