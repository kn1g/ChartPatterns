#ifndef PATTERN_DETECTOR_HPP
#define PATTERN_DETECTOR_HPP

/**
 * @file PatternDetector.hpp
 * @brief Core interface for the chart pattern detection system
 * 
 * SYSTEM OVERVIEW
 * ---------------
 * This pattern detection system implements an extensible, object-oriented framework
 * for identifying and analyzing chart patterns in financial time series data.
 * 
 * The system consists of the following key components:
 * 
 * 1. PatternDetector Interface (this file):
 *    - Defines the common interface that all pattern detectors must implement
 *    - Provides default implementations for common functionality (e.g., return calculations)
 *    - Defines data structures for storing pattern information
 * 
 * 2. Pattern-Specific Detectors (SHSDetector.cpp, ISHSDetector.cpp):
 *    - Implement the PatternDetector interface for specific patterns
 *    - Contain the pattern-specific detection, breakout, and trend logic
 *    - Currently supported patterns: Shoulder-Head-Shoulder (SHS) and inverse SHS (iSHS)
 * 
 * 3. Main Function (findPatterns.cpp):
 *    - Serves as the entry point for pattern detection
 *    - Manages detector lifecycle and orchestrates the pattern detection process
 *    - Handles data preprocessing and result formatting for R
 * 
 * 4. Utility Functions (safeLinearInterpolation.cpp):
 *    - Provides common mathematical functions used across detectors
 *    - Implements safe versions of operations to prevent numerical issues
 * 
 * DATA FLOW
 * ---------
 * 1. Input data (prices and timestamps) is passed to the system
 * 2. findPatterns creates detector instances for each requested pattern
 * 3. Each detector scans the data to identify potential patterns
 * 4. When a pattern is found, it's monitored for a breakout
 * 5. Upon breakout, performance metrics and trend information are calculated
 * 6. Results are formatted and returned to the caller
 * 
 * EXTENDING THE SYSTEM
 * -------------------
 * To add a new pattern detector:
 * 1. Create a new class that implements the PatternDetector interface
 * 2. Implement the required methods (detect, detectBreakout, getName)
 * 3. Add the new detector to the detector creation in findPatterns.cpp
 * 
 * PERFORMANCE CONSIDERATIONS
 * -------------------------
 * - Pattern detection uses early rejection conditions to minimize computation
 * - Memory pre-allocation is used to prevent frequent reallocations
 * - Pattern count is dynamically calculated based on input data size
 * - Return calculation optimizations prevent redundant passes through the data
 */

#include <Rcpp.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <fstream>
#include <atomic>
#include <iostream>

using namespace Rcpp;

// Instead of anonymous namespace, declare global variable with external linkage
// This ensures a single instance across all files
extern std::atomic<int> g_patternDataCount;

// Declare global tracking function with external linkage
extern void trackPatternDataAllocation(bool isAllocation);

// --------------------------------------------------------------------------
// Global Constants - Used across pattern detection implementations
// --------------------------------------------------------------------------
// Note: Pattern count is calculated dynamically based on input data size.
// We don't need a hardcoded constant for this anymore.
const int INVALID_TIME = -1;       // Sentinel value used to indicate an invalid or unset time value

// --------------------------------------------------------------------------
// Forward Declarations - Helper functions used by pattern detectors
// --------------------------------------------------------------------------
// Calculates a y-value at a given x position using linear interpolation between two points
// with division-by-zero protection
// Implementation is in safeLinearInterpolation.cpp and exported to R
double safeLinearInterpolation(double x1, double x2, double y1, double y2, double x);

// Calculates the slope between two points (used for various feature calculations)
double getSlope(double x1, double x2, double y1, double y2);

// Forward declaration of PatternDetector class to avoid circular reference
class PatternDetector;

// --------------------------------------------------------------------------
// Pattern Data Structure - Holds all information about a detected pattern
// --------------------------------------------------------------------------
struct PatternData {
    // Key indices marking important points in the pattern
    int startIdx;                   // First point of pattern
    int leftShoulderIdx;            // Left shoulder index
    int necklineStartIdx;           // Start of neckline index
    int headIdx;                    // Head index (highest/lowest point)
    int necklineEndIdx;             // End of neckline index
    int rightShoulderIdx;           // Right shoulder index
    int breakoutIdx;                // Breakout point index (when price crosses the neckline)
    
    std::string patternName;        // Name of the pattern ("SHS" or "iSHS")
    std::vector<int> timeStamps;    // Time points of all key positions in the pattern
    std::vector<double> priceStamps; // Price points of all key positions in the pattern
    
    // Detector reference to avoid lookups during processing
    const PatternDetector* detector; // Pointer to the detector that created this pattern
    
    // ---------- Prior Trend Information ----------
    double priorTrendStartPrice;    // Price at beginning of trend before pattern
    int priorTrendStartTime;        // Time at beginning of trend before pattern
    int priorTrendPointsCount = 0;  // Count of ascending/descending points in the prior trend
    bool priorTrendComplete = false; // Flag indicating if prior trend info is complete
    
    // ---------- Following Trend Information ----------
    double followingTrendStartPrice; // Price at beginning of trend after pattern
    int followingTrendStartTime;     // Time at beginning of trend after pattern
    int followingTrendPointsCount = 0; // Count of points in the trend following the pattern
    bool followingTrendComplete = false; // Flag indicating if following trend info is complete
    
    // Status flags for incremental returns calculation
    std::vector<bool> fixedWindowsFound;  // Tracks which fixed windows have been processed
    std::vector<bool> relWindowsFound;    // Tracks which relative windows have been processed
    
    // Performance metrics (calculated after breakout)
    std::vector<double> returns;    // Returns at fixed time windows (1,3,5,10,30,60 periods)
    std::vector<double> relReturns; // Returns at relative time windows (1/3, 1/2, 1, 2, 4 times pattern length)
    
    // Processing status flag
    bool processed = false;         // Flag indicating if this pattern has been processed
    
    // Constructor with proper initialization of all fields
    PatternData() : 
        // Initialize all indices to invalid value
        startIdx(-1), 
        leftShoulderIdx(-1), 
        necklineStartIdx(-1), 
        headIdx(-1), 
        necklineEndIdx(-1), 
        rightShoulderIdx(-1), 
        breakoutIdx(NA_INTEGER),  // Use NA_INTEGER from R for breakout
        
        // Initialize pattern name to empty string
        patternName(""), 
        
        // Initialize detector to nullptr
        detector(nullptr), 
        
        // Initialize trend information
        priorTrendStartPrice(NA_REAL), 
        priorTrendStartTime(INVALID_TIME), 
        priorTrendPointsCount(0), 
        priorTrendComplete(false),
        followingTrendStartPrice(NA_REAL), 
        followingTrendStartTime(INVALID_TIME), 
        followingTrendPointsCount(0), 
        followingTrendComplete(false),
        
        // Initialize processing status
        processed(false)
    {
        try {
            // Track allocation
            trackPatternDataAllocation(true);
            
            // Pre-allocate vectors with exact sizes needed for pattern points
            // This prevents reallocations and ensures consistent access
            timeStamps.resize(7, 0);       // 6 pattern points + breakout point
            priceStamps.resize(7, 0.0);    // 6 pattern points + breakout point
            
            // Initialize return vectors with proper sizes and NA values
            returns.resize(6, NA_REAL);    // 6 fixed windows
            relReturns.resize(5, NA_REAL); // 5 relative windows
            
            // Initialize tracking arrays
            fixedWindowsFound.resize(6, false); // 6 fixed windows
            relWindowsFound.resize(5, false);   // 5 relative windows
        }
        catch (const std::exception& e) {
            // Ensure vectors have at least empty state
            timeStamps.clear();
            priceStamps.clear();
            returns.clear();
            relReturns.clear();
            fixedWindowsFound.clear();
            relWindowsFound.clear();
        }
    }
    
    // Destructor
    ~PatternData() {
        // Call global tracking function to decrement counter
        trackPatternDataAllocation(false);
    }
    
    // Copy constructor to properly handle vector copying
    PatternData(const PatternData& other) :
        // Copy primitive fields
        startIdx(other.startIdx),
        leftShoulderIdx(other.leftShoulderIdx),
        necklineStartIdx(other.necklineStartIdx),
        headIdx(other.headIdx),
        necklineEndIdx(other.necklineEndIdx),
        rightShoulderIdx(other.rightShoulderIdx),
        breakoutIdx(other.breakoutIdx),
        patternName(other.patternName),
        detector(other.detector),
        // Deep copy vectors
        timeStamps(other.timeStamps),
        priceStamps(other.priceStamps),
        // Copy trend data
        priorTrendStartPrice(other.priorTrendStartPrice),
        priorTrendStartTime(other.priorTrendStartTime),
        priorTrendPointsCount(other.priorTrendPointsCount),
        priorTrendComplete(other.priorTrendComplete),
        followingTrendStartPrice(other.followingTrendStartPrice),
        followingTrendStartTime(other.followingTrendStartTime),
        followingTrendPointsCount(other.followingTrendPointsCount),
        followingTrendComplete(other.followingTrendComplete),
        // Copy return vectors and flags
        returns(other.returns),
        relReturns(other.relReturns),
        fixedWindowsFound(other.fixedWindowsFound),
        relWindowsFound(other.relWindowsFound),
        // Copy processing flag
        processed(other.processed)
    {
        // Track this new object
        trackPatternDataAllocation(true);
    }
    
    // Move constructor for efficient vector operations
    PatternData(PatternData&& other) noexcept :
        // Move primitive fields
        startIdx(other.startIdx),
        leftShoulderIdx(other.leftShoulderIdx),
        necklineStartIdx(other.necklineStartIdx),
        headIdx(other.headIdx),
        necklineEndIdx(other.necklineEndIdx),
        rightShoulderIdx(other.rightShoulderIdx),
        breakoutIdx(other.breakoutIdx),
        patternName(std::move(other.patternName)),
        detector(other.detector),
        // Move vectors efficiently
        timeStamps(std::move(other.timeStamps)),
        priceStamps(std::move(other.priceStamps)),
        // Move trend data
        priorTrendStartPrice(other.priorTrendStartPrice),
        priorTrendStartTime(other.priorTrendStartTime),
        priorTrendPointsCount(other.priorTrendPointsCount),
        priorTrendComplete(other.priorTrendComplete),
        followingTrendStartPrice(other.followingTrendStartPrice),
        followingTrendStartTime(other.followingTrendStartTime),
        followingTrendPointsCount(other.followingTrendPointsCount),
        followingTrendComplete(other.followingTrendComplete),
        // Move return vectors and flags
        returns(std::move(other.returns)),
        relReturns(std::move(other.relReturns)),
        fixedWindowsFound(std::move(other.fixedWindowsFound)),
        relWindowsFound(std::move(other.relWindowsFound)),
        // Move processing flag
        processed(other.processed)
    {
        // Track this new object
        trackPatternDataAllocation(true);
    }
    
    // Copy assignment operator
    PatternData& operator=(const PatternData& other) {
        if (this != &other) {
            // Copy primitive fields
            startIdx = other.startIdx;
            leftShoulderIdx = other.leftShoulderIdx;
            necklineStartIdx = other.necklineStartIdx;
            headIdx = other.headIdx;
            necklineEndIdx = other.necklineEndIdx;
            rightShoulderIdx = other.rightShoulderIdx;
            breakoutIdx = other.breakoutIdx;
            patternName = other.patternName;
            detector = other.detector;
            priorTrendStartPrice = other.priorTrendStartPrice;
            priorTrendStartTime = other.priorTrendStartTime;
            priorTrendPointsCount = other.priorTrendPointsCount;
            priorTrendComplete = other.priorTrendComplete;
            followingTrendStartPrice = other.followingTrendStartPrice;
            followingTrendStartTime = other.followingTrendStartTime;
            followingTrendPointsCount = other.followingTrendPointsCount;
            followingTrendComplete = other.followingTrendComplete;
            processed = other.processed;
            
            // Deep copy all vectors
            timeStamps = other.timeStamps;
            priceStamps = other.priceStamps;
            returns = other.returns;
            relReturns = other.relReturns;
            fixedWindowsFound = other.fixedWindowsFound;
            relWindowsFound = other.relWindowsFound;

            // Note: For assignments we DON'T call trackPatternDataAllocation
            // since the object already exists and is already counted
        }
        return *this;
    }
    
    // Move assignment operator
    PatternData& operator=(PatternData&& other) noexcept {
        if (this != &other) {
            // Move primitive fields
            startIdx = other.startIdx;
            leftShoulderIdx = other.leftShoulderIdx;
            necklineStartIdx = other.necklineStartIdx;
            headIdx = other.headIdx;
            necklineEndIdx = other.necklineEndIdx;
            rightShoulderIdx = other.rightShoulderIdx;
            breakoutIdx = other.breakoutIdx;
            patternName = std::move(other.patternName);
            detector = other.detector;
            priorTrendStartPrice = other.priorTrendStartPrice;
            priorTrendStartTime = other.priorTrendStartTime;
            priorTrendPointsCount = other.priorTrendPointsCount;
            priorTrendComplete = other.priorTrendComplete;
            followingTrendStartPrice = other.followingTrendStartPrice;
            followingTrendStartTime = other.followingTrendStartTime;
            followingTrendPointsCount = other.followingTrendPointsCount;
            followingTrendComplete = other.followingTrendComplete;
            processed = other.processed;
            
            // Move vectors efficiently
            timeStamps = std::move(other.timeStamps);
            priceStamps = std::move(other.priceStamps);
            returns = std::move(other.returns);
            relReturns = std::move(other.relReturns);
            fixedWindowsFound = std::move(other.fixedWindowsFound);
            relWindowsFound = std::move(other.relWindowsFound);
            
            // Note: For assignments we DON'T call trackPatternDataAllocation
            // since the object already exists and is already counted
        }
        return *this;
    }
    
    // Gets the index of a specific point in the pattern
    // pointNumber: 0=start, 1=leftShoulder, 2=necklineStart, 3=head, 4=necklineEnd, 5=rightShoulder
    int getPointIndex(int pointNumber) const {
        switch (pointNumber) {
            case 0: return startIdx;
            case 1: return leftShoulderIdx;
            case 2: return necklineStartIdx;
            case 3: return headIdx;
            case 4: return necklineEndIdx;
            case 5: return rightShoulderIdx;
            default: return -1;
        }
    }
    
    // Gets the breakout index
    int getBreakoutIndex() const {
        return breakoutIdx;
    }
    
    // ---------- Prior Trend Setter Methods ----------
    
    // Sets the prior trend start price
    void setPriorTrendStartPrice(double price) {
        priorTrendStartPrice = price;
    }
    
    // Sets the prior trend start time
    void setPriorTrendStartTime(int time) {
        priorTrendStartTime = time;
    }
    
    // Sets the count of trend points in the prior trend
    void setPriorTrendInfo(int count) {
        priorTrendPointsCount = count;
    }

    // Marks prior trend as complete
    void markPriorTrendComplete() {
        priorTrendComplete = true;
    }
    
    // ---------- Following Trend Setter Methods ----------
    
    // Sets the following trend start price
    void setFollowingTrendStartPrice(double price) {
        followingTrendStartPrice = price;
    }
    
    // Sets the following trend start time
    void setFollowingTrendStartTime(int time) {
        followingTrendStartTime = time;
    }
    
    // Sets the count of trend points in the following trend
    void setFollowingTrendInfo(int count) {
        followingTrendPointsCount = count;
    }

    // Marks following trend as complete
    void markFollowingTrendComplete() {
        followingTrendComplete = true;
    }
};

/**
 * @class PatternDetector
 * @brief Abstract base class for all pattern detectors
 * 
 * This class defines the common interface that all pattern detectors must implement.
 * It provides pure virtual methods for pattern detection, breakout detection, and
 * pattern invalidation. It also provides default implementations for utility methods.
 * 
 * Pattern-specific detectors (SHSDetector, ISHSDetector) inherit from this class
 * and provide the pattern-specific implementation details.
 */
class PatternDetector {
public:
    // Virtual destructor for proper cleanup in derived classes
    virtual ~PatternDetector() = default;
    
    /**
     * @brief Detect a pattern at the given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Index to check for pattern
     * @param outPattern Structure to be filled with pattern data if detected
     * @return True if pattern is detected, false otherwise
     */
    virtual bool detect(const NumericVector& prices, const NumericVector& times, 
                       int position, PatternData& outPattern) const = 0;
    
    /**
     * @brief Detect pattern breakout at the given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Current position to check for breakout
     * @param pattern Previously detected pattern data
     * @return True if a breakout is detected, false otherwise
     */
    virtual bool detectBreakout(const NumericVector& prices, const NumericVector& times,
                              int position, PatternData& pattern) const = 0;
    
    /**
     * @brief Check if pattern is invalidated at the given position
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param position Current position to check for invalidation
     * @param pattern Previously detected pattern data
     * @return True if the pattern is invalidated, false otherwise
     */
    virtual bool isPatternInvalidated(const NumericVector& prices, const NumericVector& times,
                                   int position, PatternData& pattern) const = 0;
    
    // Get the name of the pattern
    virtual std::string getName() const = 0;
    
    /**
     * @brief Update return metrics for the pattern at the given position
     * Default implementation that can be overridden by specific detectors
     * 
     * @param prices Vector of price values
     * @param times Vector of corresponding timestamps
     * @param currentPosition Current position to update returns
     * @param pattern Pattern data to update
     * @return True if all returns have been calculated, false otherwise
     */
    virtual bool updateReturns(const NumericVector& prices, const NumericVector& times,
                             int currentPosition, PatternData& pattern) const {
        // Default implementation - can be overridden by pattern-specific detectors
        return false;
    }
};

#endif // PATTERN_DETECTOR_HPP 