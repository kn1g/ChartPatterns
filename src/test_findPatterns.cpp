#include <vector>
#include <string>
#include <cmath>
#include <iostream>

// Include R headers as needed
#include <Rcpp.h>
#include "PatternDetector.hpp"

// Forward declare the findPatterns function
Rcpp::List findPatterns(Rcpp::IntegerVector, Rcpp::NumericVector, Rcpp::NumericVector);

int main() {
    std::cout << "Starting findPatterns test program..." << std::endl;
    
    // Create sample data for testing
    Rcpp::IntegerVector indices = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
    Rcpp::NumericVector times = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                                41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
                                61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                                81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
                                101, 102, 103, 104, 105, 106, 107, 108, 109, 110};
                                
    // Create a simple SHS pattern in the price data
    Rcpp::NumericVector prices(110);
    // Fill with a simple pattern - uptrend, SHS pattern, then downtrend
    for (int i = 0; i < 30; i++) {
        prices[i] = 100 + i*2; // Uptrend
    }
    // Left shoulder
    prices[30] = 160;
    prices[31] = 165;
    prices[32] = 170;
    prices[33] = 165;
    prices[34] = 160;
    // Head
    prices[35] = 155;
    prices[36] = 160;
    prices[37] = 170;
    prices[38] = 180;
    prices[39] = 175;
    prices[40] = 165;
    // Right shoulder
    prices[41] = 160;
    prices[42] = 165;
    prices[43] = 170;
    prices[44] = 165;
    prices[45] = 160;
    // Breakout and downtrend
    for (int i = 46; i < 110; i++) {
        prices[i] = 160 - (i-45)*2; // Downtrend after breakout
    }
    
    try {
        std::cout << "Calling findPatterns..." << std::endl;
        Rcpp::List result = findPatterns(indices, times, prices);
        std::cout << "findPatterns completed successfully!" << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Exception caught: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Unknown exception caught" << std::endl;
        return 1;
    }
    
    std::cout << "Test completed successfully" << std::endl;
    return 0;
} 