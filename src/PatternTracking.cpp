#include "PatternDetector.hpp"

// Define the global atomic counter for PatternData objects
// This is the actual definition that all files will link to
std::atomic<int> g_patternDataCount(0);

// Implementation of the global tracking function
void trackPatternDataAllocation(bool isAllocation) {
    if (isAllocation) {
        g_patternDataCount++;
    } else {
        g_patternDataCount--;
    }
} 