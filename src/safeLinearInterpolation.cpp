#include "PatternDetector.hpp"
#include <cmath>

/**
 * @file safeLinearInterpolation.cpp
 * @brief Safe implementation of linear interpolation for financial time series
 * 
 * Linear interpolation is a method of curve fitting using linear polynomials to
 * construct new data points within the range of a discrete set of known data points.
 * 
 * In the context of financial chart patterns, linear interpolation is primarily used to:
 * 1. Calculate neckline values at specific positions
 * 2. Determine if prices have broken through a trendline
 * 3. Project pattern completion targets
 * 
 * This implementation handles edge cases safely, particularly avoiding division by zero
 * which can occur when x1 == x2.
 */

/**
 * @brief Safely computes a point on a line between two known points
 * 
 * Formula: y = y1 + (x - x1) * (y2 - y1) / (x2 - x1)
 * 
 * This is a robust implementation that handles the edge case where x1 and x2
 * are very close or equal, which would normally cause a division by zero error.
 * 
 * @param x1 X-coordinate of the first point
 * @param x2 X-coordinate of the second point
 * @param y1 Y-coordinate of the first point
 * @param y2 Y-coordinate of the second point
 * @param x X-coordinate where we want to find the Y value
 * @return The interpolated Y value at position x
 */
double safeLinearInterpolation(double x1, double x2, double y1, double y2, double x) {
    // Check for potential division by zero
    // We use a small epsilon value (1e-10) to account for floating-point precision
    if (fabs(x2 - x1) < 1e-10) {
        // If x1 and x2 are essentially the same point (vertical line case),
        // return the average of y1 and y2 to avoid division by zero
        return (y1 + y2) / 2.0;
    }
    
    // Standard linear interpolation formula
    // This calculates a point on the line between (x1,y1) and (x2,y2)
    // The calculation is based on the slope-intercept form of a line: y = mx + b
    // Where m = (y2-y1)/(x2-x1) and b = y1 - m*x1
    double interpolatedValue = y1 + (x - x1) * (y2 - y1) / (x2 - x1);
    
    return interpolatedValue;
}

/**
 * @brief Extrapolation helper function for extending trendlines
 * 
 * This function is similar to linearInterpolation but is specifically named
 * to indicate that it can be used for extrapolation (finding points outside
 * the range of the known data points).
 * 
 * When using for extrapolation, be aware that reliability decreases
 * the further you extrapolate from known data points.
 * 
 * @param x1 X-coordinate of the first point
 * @param x2 X-coordinate of the second point
 * @param y1 Y-coordinate of the first point
 * @param y2 Y-coordinate of the second point
 * @param x X-coordinate where we want to find the Y value
 * @return The extrapolated Y value at position x
 */
double safeLinearExtrapolation(double x1, double x2, double y1, double y2, double x) {
    // Reuse the same implementation as interpolation
    // (mathematically, extrapolation uses the same formula)
    return safeLinearInterpolation(x1, x2, y1, y2, x);
}

/**
 * @brief Calculates the slope of a line between two points
 * 
 * The slope represents the "steepness" of the line and is a key indicator
 * when analyzing financial patterns, especially for determining trend direction.
 * 
 * @param x1 X-coordinate of the first point
 * @param x2 X-coordinate of the second point
 * @param y1 Y-coordinate of the first point
 * @param y2 Y-coordinate of the second point
 * @return The slope of the line, or 0 if points are too close on x-axis
 */
double calculateSlope(double x1, double x2, double y1, double y2) {
    // Check for potential division by zero
    if (fabs(x2 - x1) < 1e-10) {
        // If x1 and x2 are too close, return 0 to indicate a near-vertical line
        // This is a safety measure to prevent returning infinity
        return 0.0;
    }
    
    // Return the slope: rise over run
    return (y2 - y1) / (x2 - x1);
}

//' @name safeLinearInterpolation
//' @title Safe Linear Interpolation
//' @description Twodimensional linear interpolation for a specific point with division-by-zero protection
//' @param x1 First x coordinate
//' @param x2 Second x coordinate
//' @param y1 First y coordinate corresponding to x1
//' @param y2 Second y coordinate corresponding to x2
//' @param atPosition The point at which to calculate the interpolated value
//' @return Returns the linear interpolated y-value for the specific point
//' @examples
//' safeLinearInterpolation(1, 2, 10, 20, 1.5) # Returns 15
//'
//' @export
// [[Rcpp::export]]
double safeLinearInterpolation(double x1, double x2, double y1, double y2, double atPosition) {
    // Fallback if x2==x1 to prevent division by zero
    if (std::fabs(x2 - x1) < 1e-15) {
        return (y1 + y2) / 2.0;
    }
    
    double slope = (y2 - y1) / (x2 - x1);
    return y1 + slope * (atPosition - x1);
} 