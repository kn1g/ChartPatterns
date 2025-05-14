# ChartPatterns

## Overview

ChartPatterns is an R package with a C++ core that implements an extensible, object-oriented framework for identifying and analyzing chart patterns in financial time series data. The system focuses on detecting Shoulder-Head-Shoulder (SHS) and inverse SHS (iSHS) patterns, analyzing their validity through breakout detection, and evaluating their performance through various metrics.

## System Architecture

The pattern detection system consists of the following key components:

1. **PatternDetector Interface** (PatternDetector.hpp):
   - Defines the common interface that all pattern detectors must implement
   - Provides default implementations for common functionality (e.g., return calculations)
   - Defines data structures for storing pattern information

2. **Pattern-Specific Detectors** (SHSDetector.cpp, ISHSDetector.cpp):
   - Implement the PatternDetector interface for specific patterns
   - Contain the pattern-specific detection, breakout, and trend logic
   - Currently supported patterns: Shoulder-Head-Shoulder (SHS) and inverse SHS (iSHS)

3. **Main Function** (findPatterns.cpp):
   - Serves as the entry point for pattern detection
   - Manages detector lifecycle and orchestrates the pattern detection process
   - Handles data preprocessing and result formatting for R

4. **Utility Functions** (safeLinearInterpolation.cpp):
   - Provides common mathematical functions used across detectors
   - Implements safe versions of operations to prevent numerical issues

## How Pattern Detectors Work with Trend Tracking

### 1. Pattern Detection Process

The pattern detection process works as follows:

1. **Pattern Identification**: 
   - Detectors scan price data sequentially to find sequences matching pattern criteria
   - For SHS patterns: Looks for a sequence of high-low-high-low-high where the middle high (head) is higher than the other highs (shoulders)
   - For iSHS patterns: Looks for a sequence of low-high-low-high-low where the middle low (head) is lower than the other lows (shoulders)

2. **Pattern Validation**:
   - Once a pattern is identified, it's stored with key indices and price points
   - Patterns must satisfy geometric constraints (like shoulders being above/below necklines)

3. **Breakout Detection**:
   - After a pattern is identified, the system monitors for a breakout
   - For SHS: Breakout is when price crosses below the neckline
   - For iSHS: Breakout is when price crosses above the neckline
   - A pattern can be invalidated before breakout (e.g., if price moves beyond certain thresholds)

### 2. Trend Tracking Mechanism

Trend tracking is a key part of the pattern detection system and works as follows:

1. **Prior Trend Analysis**:
   - Before a pattern forms, the system identifies the preceding trend
   - For SHS: Looks backward for ascending lows (uptrend)
   - For iSHS: Looks backward for descending highs (downtrend)
   - This confirms the pattern is a reversal of the prior trend

2. **Following Trend Analysis**:
   - After a breakout, the system tracks the following trend
   - For SHS: Looks forward for descending highs (downtrend)
   - For iSHS: Looks forward for ascending lows (uptrend)
   - This confirms the pattern successfully predicted a trend reversal

3. **Incremental Trend Calculation**:
   - Trend calculation is performed incrementally to avoid nested loops
   - The system tracks the beginning and end of trends relative to the pattern
   - Trend information is stored with price and time for both the beginning and end points

### 3. Performance Metrics Calculation

After a pattern is formed and a breakout occurs, the system calculates performance metrics:

1. **Fixed Window Returns**:
   - Returns at fixed time intervals (1, 3, 5, 10, 30, and 60 periods after breakout)
   
2. **Relative Window Returns**:
   - Returns at relative time intervals (1/3, 1/2, 1, 2, and 4 times the pattern length)
   
3. **Return Calculation Logic**:
   - For SHS (bearish): Positive returns indicate successful pattern (price falls)
   - For iSHS (bullish): Positive returns indicate successful pattern (price rises)
   - Returns are calculated as price ratios or log returns

### 4. Integration with Main Processing

In findPatterns.cpp, the system:

1. Creates detector instances for requested pattern types
2. Scans the data to detect patterns and monitor for breakouts
3. Processes valid patterns by calculating trend information and performance metrics
4. Organizes results into structured data frames for analysis
5. Returns comprehensive information about detected patterns, their features, and performance

The trend tracking mechanism is crucial for validating that the patterns are functioning as expected from a technical analysis perspective - confirming that SHS patterns indeed mark the end of uptrends and beginning of downtrends, while iSHS patterns mark the end of downtrends and beginning of uptrends.

## Extending the System

To add a new pattern detector:

1. Create a new class that implements the PatternDetector interface
2. Implement the required methods (detect, detectBreakout, getName)
3. Add the new detector to the detector creation in findPatterns.cpp

## Performance Considerations

- Pattern detection uses early rejection conditions to minimize computation
- Memory pre-allocation is used to prevent frequent reallocations
- Pattern count is dynamically calculated based on input data size
- Return calculation optimizations prevent redundant passes through the data

## Usage

Documentation for the R interface functions is available in the package documentation. 