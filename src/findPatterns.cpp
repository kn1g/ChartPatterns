#include <vector>
#include <string>
#include <cmath>
#include <Rcpp.h>
#include "PatternDetector.hpp"
#include <iostream>
#include "SHSDetector.hpp"
#include "ISHSDetector.hpp"
#include "TrendTracker.hpp"
#include <deque>
#include <memory>

using namespace Rcpp;

// Function forward declarations
Rcpp::List findPatterns(IntegerVector PrePro_indexFilter,
                        NumericVector Original_times,
                        NumericVector Original_prices);

/**
 * @file findPatterns.cpp
 * @brief Modern Object-Oriented Implementation for Chart Pattern Detection
 * 
 * This file contains the main function for detecting Shoulder-Head-Shoulder (SHS) and
 * inverted Shoulder-Head-Shoulder (iSHS) patterns in financial time series data.
 * It uses an object-oriented approach with specialized detector classes that implement
 * the PatternDetector interface, offering better separation of concerns and error handling.
 * 
 * Core features:
 * 1. Robust error handling with multiple try-catch blocks
 * 2. Memory optimization with pre-allocation based on maximum possible patterns
 * 3. Smart pointers for automatic resource management
 * 4. Strong validation to ensure pattern criteria match other implementations
 * 
 * Detection process:
 * 1. Extracts pivot points from original time series
 * 2. Uses specialized detectors to identify potential patterns
 * 3. Validates patterns through breakout detection
 * 4. Calculates trend information and return metrics
 * 5. Returns all data in a structured format compatible with R
 */

// Declare resultList at the top-level scope so it's accessible after the try-catch blocks
Rcpp::List resultList;

//' @name findPatterns
//' @title Modern Pattern Detection with Enhanced Error Handling
//' @description Detects SHS and iSHS patterns in financial time series using an object-oriented approach
//' @param PrePro_indexFilter Vector of indices identifying pivot points in the original data series
//' @param Original_times Vector with timestamps or indices for each data point
//' @param Original_prices Vector with price values corresponding to each timestamp
//' @return Returns a list containing:
//'    - patternInfo: DataFrame with pattern identification and trend information
//'    - Features2: DataFrame with pattern point timestamps and prices
//'    - Features21to40: DataFrame with return metrics
//' @details
//'   Uses specialized detector classes for each pattern type.
//'   Provides extensive error handling and debugging output.
//'   Includes memory optimization through pre-allocation.
//'   The detection criteria are consistent with other implementations.
//' @examples
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List findPatterns(IntegerVector PrePro_indexFilter,
                        NumericVector Original_times,
                        NumericVector Original_prices) {
                          
    // Function to create empty results
    auto createEmptyResults = []() {
        // Create empty but valid DataFrame structures for all return values
        Rcpp::DataFrame empty_patternInfo = Rcpp::DataFrame::create(
            Rcpp::Named("PatternName") = std::vector<std::string>(),
            Rcpp::Named("validPattern") = std::vector<bool>(),
            Rcpp::Named("firstIndexinPrePro") = std::vector<int>(),
            Rcpp::Named("firstIndexinOriginal") = std::vector<int>(),
            Rcpp::Named("breakoutIndexinOrig") = std::vector<int>()
        );
        
        Rcpp::DataFrame empty_Features2 = Rcpp::DataFrame::create(
            Rcpp::Named("timeStamp0") = std::vector<int>(),
            Rcpp::Named("priceStamp0") = std::vector<double>()
        );
        
        Rcpp::DataFrame empty_Features21to41 = Rcpp::DataFrame::create(
            Rcpp::Named("Rendite1V") = std::vector<double>()
        );
        
        return Rcpp::List::create(
            Rcpp::Named("patternInfo") = empty_patternInfo,
            Rcpp::Named("Features2") = empty_Features2,
            Rcpp::Named("Features21to40") = empty_Features21to41
        );
    };
    
    try {            
        // Input validation
        if (PrePro_indexFilter.size() < 7) {
            Rcpp::Rcout << "Error: Preprocessed index filter must have more than 6 elements for pattern detection." << std::endl;
            return Rcpp::List::create(
                Rcpp::Named("error") = "Preprocessed index filter must have more than 6 elements for pattern detection."
            );
        }
    
        // ---- Output containers ----
        // Pattern identification vectors
        std::vector<std::string> patternNames;
        std::vector<bool> validPatterns;
        
        // Pattern position vectors
        std::vector<int> firstIndexPrePro;
        std::vector<int> firstIndexOriginal;
        std::vector<int> breakoutIndices;
        
        // Timestamps for each key point
        std::vector<int> timeStamp0;
        std::vector<int> timeStamp1;
        std::vector<int> timeStamp2;
        std::vector<int> timeStamp3;
        std::vector<int> timeStamp4;
        std::vector<int> timeStamp5;
        std::vector<int> timeStampBreakout;
        
        // Prices for each key point
        std::vector<double> priceStamp0;
        std::vector<double> priceStamp1;
        std::vector<double> priceStamp2;
        std::vector<double> priceStamp3;
        std::vector<double> priceStamp4;
        std::vector<double> priceStamp5;
        std::vector<double> priceStampBreakout;
        
        // Trend information vectors
        std::vector<double> priorTrendStartPrices;
        std::vector<int> priorTrendStartTimes;
        std::vector<int> priorTrendPointsCounts;
        std::vector<double> followingTrendStartPrices;
        std::vector<int> followingTrendStartTimes;
        std::vector<int> followingTrendPointsCounts;
        
        // Returns vectors (fixed windows)
        std::vector<double> returns1;
        std::vector<double> returns3;
        std::vector<double> returns5;
        std::vector<double> returns10;
        std::vector<double> returns30;
        std::vector<double> returns60;
        
        // Returns vectors (relative windows)
        std::vector<double> relReturns13;
        std::vector<double> relReturns12;
        std::vector<double> relReturns1;
        std::vector<double> relReturns2;
        std::vector<double> relReturns4;
        
        // ---- Data preparation ----
        // Extract pivot points (PIPs) from the original dataset using Rcpp's subsetting
        NumericVector QuerySeries_times = Original_times[PrePro_indexFilter];
        NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
            
        // ---- Output container initialization ----
        // Pre-calculate maximum possible pattern count for optimal memory allocation
        // For head-and-shoulders, we need at least 6 consecutive pivot points
        int maxPossiblePatterns = QuerySeries_prices.size() / 6;

        // Don't put arbitrary limits, but catch unreasonable values that could indicate a bug
        if (maxPossiblePatterns > QuerySeries_prices.size()) {
            Rcpp::Rcout << "WARNING: Pattern count estimate exceeds data size, capping to data size" << std::endl;
            maxPossiblePatterns = QuerySeries_prices.size();
        }

        // ---- Pattern detector initialization ----
        // Use smart pointers for automatic memory management
        std::vector<std::unique_ptr<PatternDetector>> detectors;
        // Pre-allocate memory with explicit size to avoid reallocation
        detectors.reserve(2); // We know exactly how many detectors we'll add
        // Create SHS detector
        detectors.push_back(std::unique_ptr<PatternDetector>(new SHSDetector()));
        // Create iSHS detector
        detectors.push_back(std::unique_ptr<PatternDetector>(new ISHSDetector()));
        
        // Safety check before entering main loop
        if (QuerySeries_prices.size() <= 6) {
            Rcpp::Rcout << "Not enough data points to search for patterns" << std::endl;
            return createEmptyResults();
        }
        
        // -----------------------------------------------------------------
        // ---- Main pattern detection loop ----
        // -----------------------------------------------------------------
        
        // Track patterns that are being monitored for breakout/invalidation
        std::deque<std::unique_ptr<PatternData>> potentialPatterns;
        
        // Create trend tracker for global trend tracking
        TrendTracker trendTracker;
        
        // Main loop - scan through filtered price series to identify patterns
        for (size_t i = 0; i < (QuerySeries_prices.size() - 6); ++i) {
            
            // Update trend tracking with current position
            // Check if any trend was reset
            bool trendReset = trendTracker.updateTrends(QuerySeries_prices, QuerySeries_times, i);
            
            // If a trend reset occurred, update trend info for patterns that need it
            if (trendReset) {
                trendTracker.applyTrendInfoToPatterns(potentialPatterns);
            }
            
            // First phase: Check for new patterns at current position
            for (const auto& detector : detectors) {
                
                auto pattern = std::make_unique<PatternData>();
                
                // Try to detect a new pattern at the current position
                bool detected = detector->detect(QuerySeries_prices, QuerySeries_times, i, *pattern);
                
                if (detected) {
                    // Apply trend information immediately after detection
                    trendTracker.applyTrendInfo(*pattern);
                    
                    // Add to potential patterns for monitoring
                    potentialPatterns.push_back(std::move(pattern));
                }
            }
            
            // Second phase: Check for breakouts and invalidations of existing patterns
            // We use original price series for this phase
            if (!potentialPatterns.empty()) {
                // Get current position in original series
                int currentOriginalPos = PrePro_indexFilter[i];
                
                // Safety check for bounds
                if (currentOriginalPos < 0 || currentOriginalPos >= Original_prices.size()) {
                    Rcpp::Rcout << "Warning: Invalid position in original series: " << currentOriginalPos << std::endl;
                    continue;
                }
                
                // Ensure there's room for next position checks (breakout detection needs position+1)
                // This prevents out-of-bounds access in the detector methods
                if (currentOriginalPos >= Original_prices.size() - 1) {
                    continue; // Skip if we're at the last position (can't check next value)
                }
                
                // Check each potential pattern that hasn't been processed yet
                for (auto& patternPtr : potentialPatterns) {
                    auto& pattern = *patternPtr;
                    
                    // Skip already processed patterns
                    if (pattern.processed) continue;
                    
                    // Skip patterns where right shoulder position hasn't been reached
                    if (pattern.rightShoulderIdx > i) continue;
                    
                    // Get detector from pattern (stored during pattern detection)
                    const PatternDetector* detector = pattern.detector;
                    
                    // Skip if detector reference is invalid
                    if (!detector) {
                        Rcpp::Rcout << "Detector reference is invalid" << std::endl;
                        continue;
                    }
                    
                    // Check if breakout or invalidation occurs at this position
                    try {
                        // Check if pattern is invalidated at the current pivot position
                        bool isInvalidated = detector->isPatternInvalidated(
                            Original_prices, Original_times, currentOriginalPos, pattern);
                        
                        if (isInvalidated) {
                            // Pattern invalidated - mark as processed and continue
                            pattern.processed = true;
                            continue;
                        }
                        
                        // Check for breakout if pattern is still valid and breakout not detected yet
                        if (pattern.breakoutIdx == NA_INTEGER) {
                            // CRITICAL CHANGE: Instead of just checking at the current pivot position,
                            // we now check all original data points after the right shoulder
                            // This matches the reference implementation's approach
                            
                            // First, get the original index position of the right shoulder
                            int rightShoulderOrigPos = PrePro_indexFilter[pattern.rightShoulderIdx];
                            
                            // Start scanning from the bar after the right shoulder
                            // until the current position in the original series
                            for (int j = rightShoulderOrigPos + 1; 
                                 j <= currentOriginalPos && j < Original_prices.size() - 1; 
                                 ++j) {
                                
                                // First check if the pattern is invalidated at this raw data position
                                if (detector->isPatternInvalidated(Original_prices, Original_times, j, pattern)) {
                                    pattern.processed = true;
                                    break;  // Pattern invalidated, stop processing
                                }
                                
                                // Check for breakout at this raw data position
                                bool breakoutDetected = detector->detectBreakout(
                                    Original_prices, Original_times, j, pattern);
                                    
                                if (breakoutDetected) {
                                    // Calculate returns incrementally
                                    detector->updateReturns(Original_prices, Original_times, j, pattern);
                                    break;  // Breakout found, stop scanning
                                }
                            }
                        } else if (!pattern.processed) {
                            // If breakout already found but pattern not fully processed yet,
                            // continue updating returns
                            bool returnsComplete = detector->updateReturns(
                                Original_prices, Original_times, currentOriginalPos, pattern);
                                
                            // Mark as processed when all returns are calculated
                            if (returnsComplete) {
                                pattern.processed = true;
                            }
                        }
                    } catch (std::exception& e) {
                        Rcpp::Rcout << "Error during pattern validation: " << e.what() << std::endl;
                        pattern.processed = true; // Mark as processed to avoid further errors
                    } catch (...) {
                        Rcpp::Rcout << "Unknown error during pattern validation" << std::endl;
                        pattern.processed = true; // Mark as processed to avoid further errors
                    }
                }
            }
        }
        
        // Apply final trend information to any patterns that still need it
        // This handles the case where the dataset ends without a trend reset
        trendTracker.applyFinalTrendInfo(potentialPatterns);
        
        // -----------------------------------------------------------------
        // ---- Process results ----
        // -----------------------------------------------------------------
        
        // Store pattern data in output vectors
        for (const auto& patternPtr : potentialPatterns) {
            const auto& pattern = *patternPtr;
            
            // Determine if pattern is valid (has a breakout)
            bool isValid = (pattern.breakoutIdx != NA_INTEGER);
            
            // Store pattern identification
            patternNames.push_back(pattern.patternName);
            validPatterns.push_back(isValid);
            
            // Store pattern position
            firstIndexPrePro.push_back(pattern.startIdx + 1); // +1 for R indexing
            
            if (pattern.startIdx < 0 || pattern.startIdx >= PrePro_indexFilter.size()) {
                firstIndexOriginal.push_back(NA_INTEGER); // Use NA if out of bounds
            } else {
                firstIndexOriginal.push_back(PrePro_indexFilter[pattern.startIdx] + 1); // +1 for R indexing
            }
            
            breakoutIndices.push_back(pattern.breakoutIdx); // Already R-indexed
            
            // Store key points
            timeStamp0.push_back(pattern.timeStamps[0]);
            timeStamp1.push_back(pattern.timeStamps[1]);
            timeStamp2.push_back(pattern.timeStamps[2]);
            timeStamp3.push_back(pattern.timeStamps[3]);
            timeStamp4.push_back(pattern.timeStamps[4]);
            timeStamp5.push_back(pattern.timeStamps[5]);
            
            priceStamp0.push_back(pattern.priceStamps[0]);
            priceStamp1.push_back(pattern.priceStamps[1]);
            priceStamp2.push_back(pattern.priceStamps[2]);
            priceStamp3.push_back(pattern.priceStamps[3]);
            priceStamp4.push_back(pattern.priceStamps[4]);
            priceStamp5.push_back(pattern.priceStamps[5]);
            
            // Store trend information
            priorTrendStartPrices.push_back(pattern.priorTrendStartPrice);
            priorTrendStartTimes.push_back(pattern.priorTrendStartTime);
            priorTrendPointsCounts.push_back(pattern.priorTrendPointsCount);
            followingTrendStartPrices.push_back(pattern.followingTrendStartPrice);
            followingTrendStartTimes.push_back(pattern.followingTrendStartTime);
            followingTrendPointsCounts.push_back(pattern.followingTrendPointsCount);
            
            // Store breakout and return data
            if (isValid) {
                if (pattern.timeStamps.size() <= 6 || pattern.priceStamps.size() <= 6) {
                    // Use NA values as fallback
                    timeStampBreakout.push_back(NA_INTEGER);
                    priceStampBreakout.push_back(NA_REAL);
                } else {
                    // Pattern is valid - store actual values
                    timeStampBreakout.push_back(pattern.timeStamps[6]);
                    priceStampBreakout.push_back(pattern.priceStamps[6]);
                }
                
                if (pattern.returns.size() < 6 || pattern.relReturns.size() < 5) {
                    // Use NA values as fallback for all returns
                    returns1.push_back(NA_REAL);
                    returns3.push_back(NA_REAL);
                    returns5.push_back(NA_REAL);
                    returns10.push_back(NA_REAL);
                    returns30.push_back(NA_REAL);
                    returns60.push_back(NA_REAL);
                    
                    relReturns13.push_back(NA_REAL);
                    relReturns12.push_back(NA_REAL);
                    relReturns1.push_back(NA_REAL);
                    relReturns2.push_back(NA_REAL);
                    relReturns4.push_back(NA_REAL);
                } else {
                    // Store returns
                    returns1.push_back(pattern.returns[0]);
                    returns3.push_back(pattern.returns[1]);
                    returns5.push_back(pattern.returns[2]);
                    returns10.push_back(pattern.returns[3]);
                    returns30.push_back(pattern.returns[4]);
                    returns60.push_back(pattern.returns[5]);
                    
                    relReturns13.push_back(pattern.relReturns[0]);
                    relReturns12.push_back(pattern.relReturns[1]);
                    relReturns1.push_back(pattern.relReturns[2]);
                    relReturns2.push_back(pattern.relReturns[3]);
                    relReturns4.push_back(pattern.relReturns[4]);
                }
            } else {
                // Pattern is invalid - store NA values
                timeStampBreakout.push_back(NA_INTEGER);
                priceStampBreakout.push_back(NA_REAL);
                
                returns1.push_back(NA_REAL);
                returns3.push_back(NA_REAL);
                returns5.push_back(NA_REAL);
                returns10.push_back(NA_REAL);
                returns30.push_back(NA_REAL);
                returns60.push_back(NA_REAL);
                
                relReturns13.push_back(NA_REAL);
                relReturns12.push_back(NA_REAL);
                relReturns1.push_back(NA_REAL);
                relReturns2.push_back(NA_REAL);
                relReturns4.push_back(NA_REAL);
            }
        }
        
        Rcpp::Rcout << "Detection complete. Found " << patternNames.size() << " patterns." << std::endl;
        
        // Make sure we have at least one pattern before creating DataFrames
        size_t patternInfoSize = patternNames.size();
        if (patternInfoSize == 0) {
            Rcpp::Rcout << "No patterns detected. Returning empty results." << std::endl;
            return createEmptyResults();
        }
        
        // Ensure all vectors have consistent sizes to prevent "Error: vector"
        auto ensureVectorSize = [patternInfoSize](auto& vec, const std::string& name, auto padValue) {
            if (vec.size() < patternInfoSize) {
                vec.resize(patternInfoSize, padValue);
            } else if (vec.size() > patternInfoSize) {
                vec.resize(patternInfoSize);
            }
        };

        // Fix any vector size mismatches
        ensureVectorSize(validPatterns, "validPatterns", false);
        ensureVectorSize(firstIndexPrePro, "firstIndexPrePro", NA_INTEGER);
        ensureVectorSize(firstIndexOriginal, "firstIndexOriginal", NA_INTEGER);
        ensureVectorSize(breakoutIndices, "breakoutIndices", NA_INTEGER);
        ensureVectorSize(priorTrendStartPrices, "priorTrendStartPrices", NA_REAL);
        ensureVectorSize(priorTrendStartTimes, "priorTrendStartTimes", NA_INTEGER);
        ensureVectorSize(priorTrendPointsCounts, "priorTrendPointsCounts", 0);
        ensureVectorSize(followingTrendStartPrices, "followingTrendStartPrices", NA_REAL);
        ensureVectorSize(followingTrendStartTimes, "followingTrendStartTimes", NA_INTEGER);
        ensureVectorSize(followingTrendPointsCounts, "followingTrendPointsCounts", 0);

        ensureVectorSize(timeStamp0, "timeStamp0", NA_INTEGER);
        ensureVectorSize(timeStamp1, "timeStamp1", NA_INTEGER);
        ensureVectorSize(timeStamp2, "timeStamp2", NA_INTEGER);
        ensureVectorSize(timeStamp3, "timeStamp3", NA_INTEGER);
        ensureVectorSize(timeStamp4, "timeStamp4", NA_INTEGER);
        ensureVectorSize(timeStamp5, "timeStamp5", NA_INTEGER);
        ensureVectorSize(timeStampBreakout, "timeStampBreakout", NA_INTEGER);
        ensureVectorSize(priceStamp0, "priceStamp0", NA_REAL);
        ensureVectorSize(priceStamp1, "priceStamp1", NA_REAL);
        ensureVectorSize(priceStamp2, "priceStamp2", NA_REAL);
        ensureVectorSize(priceStamp3, "priceStamp3", NA_REAL);
        ensureVectorSize(priceStamp4, "priceStamp4", NA_REAL);
        ensureVectorSize(priceStamp5, "priceStamp5", NA_REAL);
        ensureVectorSize(priceStampBreakout, "priceStampBreakout", NA_REAL);

        ensureVectorSize(returns1, "returns1", NA_REAL);
        ensureVectorSize(returns3, "returns3", NA_REAL);
        ensureVectorSize(returns5, "returns5", NA_REAL);
        ensureVectorSize(returns10, "returns10", NA_REAL);
        ensureVectorSize(returns30, "returns30", NA_REAL);
        ensureVectorSize(returns60, "returns60", NA_REAL);
        ensureVectorSize(relReturns13, "relReturns13", NA_REAL);
        ensureVectorSize(relReturns12, "relReturns12", NA_REAL);
        ensureVectorSize(relReturns1, "relReturns1", NA_REAL);
        ensureVectorSize(relReturns2, "relReturns2", NA_REAL);
        ensureVectorSize(relReturns4, "relReturns4", NA_REAL);
        
        // --- Create main pattern information data frame ---
        // This contains the basic pattern metadata and trend information
        // All vectors must have the same length (number of detected patterns)
        Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
            // Pattern identification
            Rcpp::Named("PatternName") = patternNames,         // Type of pattern (SHS or iSHS)
            Rcpp::Named("validPattern") = validPatterns,       // Whether pattern had a breakout (true/false)
            
            // Pattern position indices
            Rcpp::Named("firstIndexinPrePro") = firstIndexPrePro,          // Start index in preprocessed data
            Rcpp::Named("firstIndexinOriginal") = firstIndexOriginal,      // Start index in original data
            Rcpp::Named("breakoutIndexinOrig") = breakoutIndices,          // Breakout index in original data
            
            // Trend context information
            Rcpp::Named("TrendBeginnPreis") = priorTrendStartPrices,  // Price at beginning of preceding trend
            Rcpp::Named("TrendBeginnZeit") = priorTrendStartTimes,    // Time at beginning of preceding trend
            Rcpp::Named("TrendPointsCount") = priorTrendPointsCounts,  // Count of trend points
            Rcpp::Named("TrendBeginnPreisFollowing") = followingTrendStartPrices,  // Price at beginning of following trend
            Rcpp::Named("TrendBeginnZeitFollowing") = followingTrendStartTimes,    // Time at beginning of following trend
            Rcpp::Named("TrendPointsCountFollowing") = followingTrendPointsCounts  // Count of trend points
        );

        
        // --- Create pattern feature points data frame ---
        // This contains timestamps and prices for all key points in each pattern
        // Used for visualization and detailed analysis of pattern structure
        Rcpp::DataFrame Features2 = Rcpp::DataFrame::create(
            // Timestamps for all key pattern points
            Rcpp::Named("timeStamp0") = timeStamp0,              // First point timestamp
            Rcpp::Named("timeStamp1") = timeStamp1,              // Left shoulder timestamp
            Rcpp::Named("timeStamp2") = timeStamp2,              // First peak/trough timestamp
            Rcpp::Named("timeStamp3") = timeStamp3,              // Head timestamp
            Rcpp::Named("timeStamp4") = timeStamp4,              // Second peak/trough timestamp
            Rcpp::Named("timeStamp5") = timeStamp5,              // Right shoulder timestamp
            Rcpp::Named("timeStampBreakOut") = timeStampBreakout,// Breakout point timestamp
            
            // Prices for all key pattern points
            Rcpp::Named("priceStamp0") = priceStamp0,            // First point price
            Rcpp::Named("priceStamp1") = priceStamp1,            // Left shoulder price
            Rcpp::Named("priceStamp2") = priceStamp2,            // First peak/trough price
            Rcpp::Named("priceStamp3") = priceStamp3,            // Head price
            Rcpp::Named("priceStamp4") = priceStamp4,            // Second peak/trough price
            Rcpp::Named("priceStamp5") = priceStamp5,            // Right shoulder price
            Rcpp::Named("priceStampBreakOut") = priceStampBreakout // Breakout point price
        );
        
        // --- Create pattern performance metrics data frame ---
        // This contains all return data for analyzing pattern profitability
        Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
            // Fixed time window returns (periods after breakout)
            Rcpp::Named("Rendite1V") = returns1,       // Return 1 period after breakout
            Rcpp::Named("Rendite3V") = returns3,       // Return 3 periods after breakout
            Rcpp::Named("Rendite5V") = returns5,       // Return 5 periods after breakout
            Rcpp::Named("Rendite10V") = returns10,     // Return 10 periods after breakout
            Rcpp::Named("Rendite30V") = returns30,     // Return 30 periods after breakout
            Rcpp::Named("Rendite60V") = returns60,     // Return 60 periods after breakout
            
            // Relative time window returns (multiples of pattern length)
            Rcpp::Named("relRendite13V") = relReturns13, // Return after 1/3 of pattern length
            Rcpp::Named("relRendite12V") = relReturns12, // Return after 1/2 of pattern length
            Rcpp::Named("relRendite1V") = relReturns1,   // Return after 1x pattern length
            Rcpp::Named("relRendite2V") = relReturns2,   // Return after 2x pattern length
            Rcpp::Named("relRendite4V") = relReturns4    // Return after 4x pattern length
        );
        
        // Store results
        Rcpp::List resultList = Rcpp::List::create(
            Rcpp::Named("patternInfo") = patternInfo,     // Basic pattern information and trend context
            Rcpp::Named("Features2") = Features2,         // Pattern point timestamps and prices
            Rcpp::Named("Features21to40") = Features21to41 // Pattern performance metrics (returns)
        );
        
        // Return the final results
        return resultList;
        
    } catch (const std::exception& e) {
        Rcpp::Rcout << "Error: " << e.what() << std::endl;
        return createEmptyResults();
    } catch (...) {
        Rcpp::Rcout << "Unknown error" << std::endl;
        return createEmptyResults();
    }
} 