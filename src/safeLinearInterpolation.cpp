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
 * @brief Extrapolation helper function for extending trendlines
 *
 * This function uses the same logic as interpolation but is intended for cases
 * where the target x is outside the known x range.
 *
 * @param x1 X-coordinate of the first point
 * @param x2 X-coordinate of the second point
 * @param y1 Y-coordinate of the first point
 * @param y2 Y-coordinate of the second point
 * @param x X-coordinate where we want to find the Y value
 * @return The extrapolated Y value at position x
 */
double safeLinearExtrapolation(double x1, double x2, double y1, double y2, double x) {
  return safeLinearInterpolation(x1, x2, y1, y2, x);
}

/**
 * @brief Calculates the slope of a line between two points
 *
 * @param x1 X-coordinate of the first point
 * @param x2 X-coordinate of the second point
 * @param y1 Y-coordinate of the first point
 * @param y2 Y-coordinate of the second point
 * @return The slope of the line, or 0 if x1 and x2 are nearly equal
 */
double calculateSlope(double x1, double x2, double y1, double y2) {
  if (fabs(x2 - x1) < 1e-10) {
    return 0.0;
  }
  
  return (y2 - y1) / (x2 - x1);
}

//' @name safeLinearInterpolation
 //' @title Safe Linear Interpolation
 //' @description Two-dimensional linear interpolation for a specific point with division-by-zero protection
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
   if (std::fabs(x2 - x1) < 1e-15) {
     return (y1 + y2) / 2.0;
   }
   
   double slope = (y2 - y1) / (x2 - x1);
   return y1 + slope * (atPosition - x1);
 }
