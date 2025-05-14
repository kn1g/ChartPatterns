#ifndef ISHSDETECTOR_HPP
#define ISHSDETECTOR_HPP

#include "PatternDetector.hpp"
#include <vector>

/**
 * @file ISHSDetector.hpp
 * @brief Header for the Inverse Shoulder-Head-Shoulder (iSHS) pattern detector
 * 
 * The iSHS pattern is a bullish reversal pattern that forms after a downtrend.
 * It consists of:
 * 1. A trough (left shoulder)
 * 2. A lower trough (head)
 * 3. A higher trough (right shoulder)
 * 4. A neckline connecting the peaks between the troughs
 * 
 * The pattern is confirmed when price breaks above the neckline.
 */

class ISHSDetector : public PatternDetector {
public:
    /**
     * @brief Default constructor
     */
    ISHSDetector() {}
    
    /**
     * @brief Detect ISHS pattern at given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Index to start checking for pattern
     * @param outPattern Structure to be filled with pattern data if detected
     * @return True if ISHS pattern is detected, false otherwise
     */
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override;
    
    /**
     * @brief Detect ISHS pattern breakout at given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Current position to check for breakout
     * @param pattern Previously detected pattern data
     * @return True if a breakout is detected, false otherwise
     */
    bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                        int position, PatternData& pattern) const override;
    
    /**
     * @brief Get the pattern name
     * 
     * @return "iSHS"
     */
    std::string getName() const override;
    
    /**
     * @brief Check if ISHS pattern is invalidated at given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Current position to check for invalidation
     * @param pattern Previously detected pattern data
     * @return True if the pattern is invalidated, false otherwise
     */
    bool isPatternInvalidated(const NumericVector& prices, const NumericVector& times,
                              int position, PatternData& pattern) const override;
    
    /**
     * @brief Update return metrics for ISHS pattern at given position
     * Incremental approach that avoids nested loops
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param currentPosition Current position to update returns
     * @param pattern Pattern data to update
     * @return True if all returns have been calculated, false otherwise
     */
    bool updateReturns(const NumericVector& prices, const NumericVector& times,
                     int currentPosition, PatternData& pattern) const override;
    
};

#endif // ISHSDETECTOR_HPP 