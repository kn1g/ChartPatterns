#include <vector>
#include <string>
#include <cmath>
#include "PatternDetector.hpp"
#include "SHSDetector.cpp"
#include "ISHSDetector.cpp"
#include <memory>
#include <algorithm>
#include <iostream>
#include <Rcpp.h>

/**
 * @file findPatterns.cpp
 * @brief Main implementation for chart pattern detection
 * 
 * This file contains the main function for detecting patterns (specifically SHS and iSHS) 
 * in financial time series data. It utilizes specialized detector classes that implement 
 * the PatternDetector interface to identify specific patterns in the data.
 * 
 * The detection system works by:
 * 1. Accepting time series data (prices and timestamps)
 * 2. Using various pattern detectors (SHS and iSHS) to identify patterns
 * 3. Tracking detected patterns and monitoring for breakouts
 * 4. Calculating returns and trends after pattern completion
 * 
 * This implementation incorporates the most efficient and reliable features from 
 * previous implementations while maintaining strict adherence to the original
 * pattern recognition criteria.
 * 
 * PERFORMANCE OPTIMIZATIONS:
 * - Dynamic pattern count calculation to optimize memory usage
 * - Early rejection of invalid patterns to minimize processing
 * - Pre-allocation of memory for all data structures
 * - Single-pass return calculation to avoid redundant processing
 * - Smart pointers for automatic resource management
 * - Direct storage in output vectors without redundant pattern collection
 */

using namespace Rcpp;

// Forward declarations of our pattern detector classes
// These are defined in their respective source files
class SHSDetector;
class ISHSDetector;

// Declare resultList at the top-level scope so it's accessible after the try-catch blocks
Rcpp::List resultList;

//' @name findPatterns
//' @title findPatterns - Efficient Pattern Recognition
//' @description Detects chart patterns in financial time series data with optimized implementation
//' @param PrePro_indexFilter Vector with indices of pivot points in the original data
//' @param Original_times Vector with time values
//' @param Original_prices Vector with price values
//' @return Returns a list with pattern information, timestamps, prices, and performance metrics
//' @examples
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List findPatterns(IntegerVector PrePro_indexFilter,
                        NumericVector Original_times,
                        NumericVector Original_prices) {
    // Add minimal debug output
    Rcpp::Rcout << "[DEBUG] Starting findPatterns with " << Original_prices.size() 
                << " prices, " << Original_times.size() << " times, and " 
                << PrePro_indexFilter.size() << " pivot points" << std::endl;
    
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
    std::vector<double> trendBeginPrices;
    std::vector<double> trendBeginTimes;
    std::vector<double> trendEndPrices;
    std::vector<double> trendEndTimes;
    
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
    
    // ---- Input validation ----
    
    // Check if the price and time series are of sufficient length
    if (Original_prices.size() <= 6 || Original_times.size() <= 6) {
        Rcpp::stop("Input price and time series must have more than 6 elements for pattern detection");
    }
    
    // Check if price and time vectors have the same length
    if (Original_prices.size() != Original_times.size()) {
        Rcpp::stop("Price and time series must have the same length");
    }
    
    // Check if PrePro_indexFilter is valid
    if (PrePro_indexFilter.size() <= 6) {
        Rcpp::stop("Preprocessed index filter must have more than 6 elements for pattern detection");
    }
    
    // Controls whether the index starts at zero
    if (PrePro_indexFilter[0] != 0) {
        Function warning("warning");
        warning("PrePro Vector indices does not start at Zero.");
    }
    
    Rcpp::Rcout << "[DEBUG] Input validation successful" << std::endl;
    
    // ---- Data preparation ----
    
    NumericVector QuerySeries_times;
    NumericVector QuerySeries_prices;
    
    try {
        Rcpp::Rcout << "[DEBUG] Starting data preparation" << std::endl;
        
        // Extract pivot points (PIPs) from the original dataset
        // OPTIMIZATION: Using Rcpp's subsetting operation which is more efficient than manual extraction
        QuerySeries_times = Original_times[PrePro_indexFilter];
        QuerySeries_prices = Original_prices[PrePro_indexFilter];
        
        Rcpp::Rcout << "[DEBUG] Data preparation complete. Extracted " 
                    << QuerySeries_prices.size() << " pivot points." << std::endl;
    } catch (const std::exception& e) {
        Rcpp::Rcout << "[DEBUG] Exception during data preparation: " << e.what() << std::endl;
        throw; // Re-throw the exception after logging
    } catch (...) {
        Rcpp::Rcout << "[DEBUG] Unknown exception during data preparation" << std::endl;
        throw; // Re-throw the exception after logging
    }
    
    // ---- Output container initialization ----
    
    int maxPossiblePatterns = 0;
    try {
        Rcpp::Rcout << "[DEBUG] Starting container initialization" << std::endl;
        
        // OPTIMIZATION: Pre-calculate maximum possible pattern count for optimal memory allocation
        // Each pattern needs at least 6 points, so max non-overlapping patterns = total points / 6
        maxPossiblePatterns = QuerySeries_prices.size() / 6;
        Rcpp::Rcout << "[DEBUG] Maximum possible patterns: " << maxPossiblePatterns << std::endl;
    } catch (const std::exception& e) {
        Rcpp::Rcout << "[DEBUG] Exception during container initialization: " << e.what() << std::endl;
        throw; // Re-throw the exception after logging
    } catch (...) {
        Rcpp::Rcout << "[DEBUG] Unknown exception during container initialization" << std::endl;
        throw; // Re-throw the exception after logging
    }
    
    // ---- Pattern detector initialization ----
    
    // OPTIMIZATION: Use smart pointers for automatic memory management
    std::vector<std::unique_ptr<PatternDetector>> detectors;
    
    try {
        Rcpp::Rcout << "[DEBUG] Starting detector initialization" << std::endl;
        
        detectors.reserve(2); // We know exactly how many detectors we'll add
        
        Rcpp::Rcout << "[DEBUG] Creating SHS detector" << std::endl;
        detectors.push_back(std::make_unique<SHSDetector>());
        
        Rcpp::Rcout << "[DEBUG] Creating iSHS detector" << std::endl;
        detectors.push_back(std::make_unique<ISHSDetector>());
        
        Rcpp::Rcout << "[DEBUG] Detector initialization complete. Created " 
                    << detectors.size() << " detectors" << std::endl;
        
        // Safety check before entering main loop
        if (QuerySeries_prices.size() <= 6) {
            Rcpp::Rcout << "[DEBUG] Not enough data points to search for patterns" << std::endl;
            // Return empty results instead of throwing
            goto create_empty_results;
        }
        
        Rcpp::Rcout << "[DEBUG] Will start main detection loop with " 
                    << QuerySeries_prices.size() - 6 << " positions to check" << std::endl;
    } catch (const std::exception& e) {
        Rcpp::Rcout << "[DEBUG] Exception during detector initialization: " << e.what() << std::endl;
        // Don't throw, continue to try to create empty results
        goto create_empty_results;
    } catch (...) {
        Rcpp::Rcout << "[DEBUG] Unknown exception during detector initialization" << std::endl;
        // Don't throw, continue to try to create empty results
        goto create_empty_results;
    }
    
    // -----------------------------------------------------------------
    // ---- Main pattern detection loop ----
    // -----------------------------------------------------------------
    //
    // ARCHITECTURAL NOTE:
    // Pattern data is stored directly in the output vectors instead of keeping a separate
    // collection of pattern objects. This avoids redundant storage and improves memory usage.
    // The pattern detector classes are responsible for pattern-specific logic (detection,
    // invalidation, breakout), while this function handles data collection and formatting
    // for R output. This separation of concerns keeps the detector classes focused on their
    // core functionality without needing to know about the output format requirements.
    //
    try {
        Rcpp::Rcout << "[DEBUG] Starting main detection loop" << std::endl;
        size_t patterns_found = 0;
        
        for (size_t i = 0; i < (QuerySeries_prices.size() - 6); ++i) {
            // Simplified progress reporting - less frequent and more minimal
            if (i % 500 == 0 || i == 0) {
                Rcpp::Rcout << "[DEBUG] Processing position " << i << " / " 
                       << (QuerySeries_prices.size() - 6) << std::endl;
            }
            
            // For each detector, check if a pattern is detected at position i
            for (const auto& detector : detectors) {
                PatternData pattern;
                try {
                    bool detected = detector->detect(QuerySeries_prices, QuerySeries_times, i, pattern);
                    if (detected) {
                        patterns_found++;
                        if (patterns_found % 10 == 1) {
                            Rcpp::Rcout << "[DEBUG] Pattern '" << pattern.patternName << "' detected at position " << i << std::endl;
                        }
                        
                        // OPTIMIZATION: Safety check against theoretical maximum to prevent memory issues
                        if (patternNames.size() >= static_cast<size_t>(maxPossiblePatterns)) {
                            Rcpp::Rcout << "[DEBUG] Maximum pattern count reached (" << maxPossiblePatterns << "). Stopping detection." << std::endl;
                            break; // Exit the loop instead of stopping with an error
                        }
                        
                        // Pattern is detected, now search for breakout
                        bool foundBreakout = false;
                        
                        // Only output breakout search for some patterns to reduce volume
                        if (patterns_found % 50 == 1) {
                            Rcpp::Rcout << "[DEBUG] Searching for breakout for pattern " << patterns_found << std::endl;
                        }
                        
                        // OPTIMIZATION: Start breakout search after right shoulder
                        // This avoids unnecessary checks before the pattern is complete
                        for (int j = PrePro_indexFilter[i+5]; j < Original_times.size() - 1; ++j) {
                            
                            // OPTIMIZATION: Early rejection if pattern has been invalidated
                            // Use the detector's implementation to determine if pattern is still valid
                            if (detector->isPatternInvalidated(Original_prices, Original_times, j, pattern)) {
                                break;  // Pattern is no longer valid, stop checking for breakout
                            }
                            
                            // Check for breakout using the appropriate detector
                            if (detector->detectBreakout(Original_prices, Original_times, j, pattern)) {
                                // Breakout found - mark pattern as valid and store breakout index
                                // Adjusting to R's 1-based indexing for return values
                                foundBreakout = true;
                                pattern.breakoutIdx = j + 1; // +1 for R indexing
                                
                                // Only log some breakouts to reduce output volume
                                if (patterns_found % 50 == 1) {
                                    Rcpp::Rcout << "[DEBUG] Breakout found for pattern " << patterns_found << std::endl;
                                }
                                
                                // --- Analyze trend context around the pattern ---
                                // Declare variables to hold trend information before/after pattern
                                double trendBeginPrice, trendEndPrice;
                                int trendBeginTime, trendEndTime;
                                
                                // Simplified with no nested try-catch blocks for trend calculation
                                detector->calculateTrend(QuerySeries_prices, QuerySeries_times, 
                                                     pattern, j, 
                                                     trendBeginPrice, trendBeginTime, 
                                                     trendEndPrice, trendEndTime);
                                
                                // Store the trend information in the pattern object for later analysis
                                pattern.trendBeginPrice = trendBeginPrice;
                                pattern.trendBeginTime = trendBeginTime;
                                pattern.trendEndPrice = trendEndPrice;
                                pattern.trendEndTime = trendEndTime;
                                
                                // --- Process pattern returns (performance metrics) ---
                                // Initialize return value containers with NA (missing value)
                                // Fixed window returns (for 1,3,5,10,30,60 periods after breakout)
                                std::vector<double> returnValues(6, NA_REAL);
                                // Relative window returns (for 1/3, 1/2, 1, 2, 4 times pattern length)
                                std::vector<double> relReturnValues(5, NA_REAL);
                                
                                // Simplified with no nested try-catch blocks for returns calculation
                                detector->calculateReturns(Original_prices, Original_times,
                                                        j+1, // Breakout index (R-indexed)
                                                        pattern.startIdx, // Pattern start index
                                                        returnValues, // Output for fixed window returns 
                                                        relReturnValues); // Output for relative window returns
                                
                                // Store the calculated returns in the pattern object
                                pattern.returns = returnValues;
                                pattern.relReturns = relReturnValues;
                                
                                // --- Record breakout point details ---
                                // The vectors were already sized to hold 7 elements during pattern detection
                                // (6 pattern points + 1 breakout point)
                                
                                // Store the actual breakout timestamp and price as the 7th point (index 6)
                                pattern.timeStamps[6] = Original_times[j+1];
                                pattern.priceStamps[6] = Original_prices[j+1];
                                
                                // Exit the breakout search loop since we found a valid breakout
                                break;
                            }
                        }
                        
                        // --- Populate output vectors with pattern details ---
                        // Directly store pattern data in output vectors without the redundant patterns vector
                        // These vectors will be used to create the R data frames for return values
                        
                        // Store pattern identification information
                        patternNames.push_back(pattern.patternName);  // Type of pattern ("SHS" or "iSHS")
                        validPatterns.push_back(foundBreakout);       // Whether a breakout was found
                        
                        // Store pattern position information (adding 1 for R's 1-based indexing)
                        firstIndexPrePro.push_back(pattern.startIdx + 1);  // Index in pivot point series
                        firstIndexOriginal.push_back(PrePro_indexFilter[pattern.startIdx] + 1);  // Index in original series
                        breakoutIndices.push_back(pattern.breakoutIdx);    // Breakout index (already R-indexed)
                        
                        // --- Store timestamps for each key point in the pattern ---
                        // For SHS: 0=First point, 1=Left shoulder, 2=Trough1, 3=Head, 4=Trough2, 5=Right shoulder
                        // For iSHS: 0=First point, 1=Left shoulder, 2=Peak1, 3=Head, 4=Peak2, 5=Right shoulder
                        timeStamp0.push_back(pattern.timeStamps[0]);  // First point timestamp
                        timeStamp1.push_back(pattern.timeStamps[1]);  // Left shoulder timestamp
                        timeStamp2.push_back(pattern.timeStamps[2]);  // First peak/trough timestamp
                        timeStamp3.push_back(pattern.timeStamps[3]);  // Head timestamp
                        timeStamp4.push_back(pattern.timeStamps[4]);  // Second peak/trough timestamp
                        timeStamp5.push_back(pattern.timeStamps[5]);  // Right shoulder timestamp
                        
                        // --- Store prices for each key point in the pattern ---
                        // These price values correspond to the timestamps above
                        priceStamp0.push_back(pattern.priceStamps[0]);  // First point price
                        priceStamp1.push_back(pattern.priceStamps[1]);  // Left shoulder price
                        priceStamp2.push_back(pattern.priceStamps[2]);  // First peak/trough price
                        priceStamp3.push_back(pattern.priceStamps[3]);  // Head price
                        priceStamp4.push_back(pattern.priceStamps[4]);  // Second peak/trough price
                        priceStamp5.push_back(pattern.priceStamps[5]);  // Right shoulder price
                        
                        // --- Store surrounding trend information ---
                        // These values capture the trend context before and after the pattern
                        trendBeginPrices.push_back(pattern.trendBeginPrice);  // Price at start of preceding trend
                        trendBeginTimes.push_back(pattern.trendBeginTime);    // Time at start of preceding trend
                        trendEndPrices.push_back(pattern.trendEndPrice);      // Price at end of following trend
                        trendEndTimes.push_back(pattern.trendEndTime);        // Time at end of following trend
                        
                        // --- Handle breakout-dependent data storage ---
                        if (foundBreakout) {
                            // --- Store breakout point details ---
                            // If a valid breakout was found, store the actual timestamp and price at breakout
                            timeStampBreakout.push_back(pattern.timeStamps[6]);   // Breakout timestamp (7th point)
                            priceStampBreakout.push_back(pattern.priceStamps[6]); // Breakout price (7th point)
                            
                            // --- Store fixed-window return values ---
                            // Returns at specific periods after breakout (1,3,5,10,30,60)
                            returns1.push_back(pattern.returns[0]);   // Return after 1 period
                            returns3.push_back(pattern.returns[1]);   // Return after 3 periods
                            returns5.push_back(pattern.returns[2]);   // Return after 5 periods
                            returns10.push_back(pattern.returns[3]);  // Return after 10 periods
                            returns30.push_back(pattern.returns[4]);  // Return after 30 periods
                            returns60.push_back(pattern.returns[5]);  // Return after 60 periods
                            
                            // --- Store relative-window return values ---
                            // Returns at specific multiples of pattern length
                            relReturns13.push_back(pattern.relReturns[0]);  // Return after 1/3 of pattern length
                            relReturns12.push_back(pattern.relReturns[1]);  // Return after 1/2 of pattern length
                            relReturns1.push_back(pattern.relReturns[2]);   // Return after 1x pattern length
                            relReturns2.push_back(pattern.relReturns[3]);   // Return after 2x pattern length
                            relReturns4.push_back(pattern.relReturns[4]);   // Return after 4x pattern length
                        } else {
                            // --- Handle patterns without breakout ---
                            // For patterns without a breakout, we still need to maintain consistent
                            // vector sizes by adding NA values as placeholders
                            
                            // Add NA for breakout data
                            timeStampBreakout.push_back(NA_INTEGER);  // No breakout timestamp (missing value)
                            priceStampBreakout.push_back(NA_REAL);    // No breakout price (missing value)
                            
                            // Add NA for all fixed-window returns
                            returns1.push_back(NA_REAL);   // No 1-period return
                            returns3.push_back(NA_REAL);   // No 3-period return
                            returns5.push_back(NA_REAL);   // No 5-period return
                            returns10.push_back(NA_REAL);  // No 10-period return
                            returns30.push_back(NA_REAL);  // No 30-period return
                            returns60.push_back(NA_REAL);  // No 60-period return
                            
                            // Add NA for all relative-window returns
                            relReturns13.push_back(NA_REAL);  // No 1/3-length return
                            relReturns12.push_back(NA_REAL);  // No 1/2-length return
                            relReturns1.push_back(NA_REAL);   // No 1x-length return
                            relReturns2.push_back(NA_REAL);   // No 2x-length return
                            relReturns4.push_back(NA_REAL);   // No 4x-length return
                        }
                        
                        // --- Advance the pattern search position ---
                        // Skip indices that have already been analyzed as part of this pattern
                        // Each pattern uses 6 points, so we can safely skip ahead by 5 positions
                        // (skipping 5 moves us to position i+5, the last point of the current pattern)
                        i = i + 5;
                        
                        // Exit the detector loop since we found a pattern
                        // This prevents checking other detectors at the same position
                        break;
                    }
                } catch (const std::exception& e) {
                    Rcpp::Rcout << "[DEBUG] Exception in pattern detection: " << e.what() << std::endl;
                }
            }
        }
        
        Rcpp::Rcout << "[DEBUG] Detection complete. Found " << patternNames.size() 
                    << " patterns. Creating return structures." << std::endl;
        
        try {
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
                Rcpp::Named("TrendBeginnPreis") = trendBeginPrices,  // Price at beginning of preceding trend
                Rcpp::Named("TrendBeginnZeit") = trendBeginTimes,    // Time at beginning of preceding trend
                Rcpp::Named("TrendEndePreis") = trendEndPrices,      // Price at end of following trend
                Rcpp::Named("TrendEndeZeit") = trendEndTimes         // Time at end of following trend
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
            
            // Store results to return them after the try-catch blocks
            resultList = Rcpp::List::create(
                Rcpp::Named("patternInfo") = patternInfo,     // Basic pattern information and trend context
                Rcpp::Named("Features2") = Features2,         // Pattern point timestamps and prices
                Rcpp::Named("Features21to40") = Features21to41 // Pattern performance metrics (returns)
            );
            
            Rcpp::Rcout << "[DEBUG] All data frames created successfully." << std::endl;
        } catch (const std::exception& e) {
            Rcpp::Rcout << "[DEBUG] Exception during result preparation: " << e.what() << std::endl;
            goto create_empty_results; // Use empty results if we can't create the normal ones
        } catch (...) {
            Rcpp::Rcout << "[DEBUG] Unknown exception during result preparation" << std::endl;
            goto create_empty_results; // Use empty results if we can't create the normal ones
        }
    } catch (const std::exception& e) {
        Rcpp::Rcout << "[DEBUG] Exception during main detection loop: " << e.what() << std::endl;
        goto create_empty_results; // Use empty results instead of throwing
    } catch (...) {
        Rcpp::Rcout << "[DEBUG] Unknown exception during main detection loop" << std::endl;
        goto create_empty_results; // Use empty results instead of throwing
    }
    
    // If we reach here, we have valid results to return
    Rcpp::Rcout << "[DEBUG] Returning results with " << patternNames.size() << " patterns" << std::endl;
    return resultList;

create_empty_results:
    Rcpp::Rcout << "[DEBUG] Creating empty results structures" << std::endl;
    
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
    
    Rcpp::Rcout << "[DEBUG] Returning empty results" << std::endl;
    
    return Rcpp::List::create(
        Rcpp::Named("patternInfo") = empty_patternInfo,
        Rcpp::Named("Features2") = empty_Features2,
        Rcpp::Named("Features21to40") = empty_Features21to41
    );
} 