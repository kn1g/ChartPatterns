#ifndef SHSDETECTOR_HPP
#define SHSDETECTOR_HPP

#include "PatternDetector.hpp"
#include <vector>

/**
 * @file SHSDetector.hpp
 * @brief Header for the Shoulder-Head-Shoulder (SHS) pattern detector
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

class SHSDetector : public PatternDetector {
public:
    /**
     * @brief Default constructor
     */
    SHSDetector() {}
    
    /**
     * @brief Detect SHS pattern at given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Index to start checking for pattern
     * @param outPattern Structure to be filled with pattern data if detected
     * @return True if SHS pattern is detected, false otherwise
     */
    bool detect(const NumericVector& prices, const NumericVector& times, 
                int position, PatternData& outPattern) const override;
    
    /**
     * @brief Detect SHS pattern breakout at given position
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
     * @return "SHS"
     */
    std::string getName() const override;
    
    /**
     * @brief Check if SHS pattern is invalidated at given position
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
     * @brief Update return metrics for SHS pattern at given position
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

#endif // SHSDETECTOR_HPP 