#include <vector>
#include <string>
#include <cmath>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <chrono>

// Include R headers as needed
#include <Rcpp.h>
#include "PatternDetector.hpp"

// Forward declare the implementation functions
Rcpp::List findPatterns(Rcpp::IntegerVector, Rcpp::NumericVector, Rcpp::NumericVector);
Rcpp::DataFrame FastFindII(Rcpp::IntegerVector, Rcpp::NumericVector, Rcpp::NumericVector);
Rcpp::DataFrame FastFind(Rcpp::IntegerVector, Rcpp::NumericVector, Rcpp::NumericVector);

// Function to print a row from a DataFrame for comparison
void printDataFrameRow(Rcpp::DataFrame df, int row, const std::string& prefix) {
    std::cout << prefix << " | ";
    for (int col = 0; col < df.size(); col++) {
        if (TYPEOF(df[col]) == INTSXP) {
            Rcpp::IntegerVector column = df[col];
            if (row < column.size()) {
                std::cout << std::setw(10) << column[row] << " | ";
            } else {
                std::cout << std::setw(10) << "NA" << " | ";
            }
        } else if (TYPEOF(df[col]) == REALSXP) {
            Rcpp::NumericVector column = df[col];
            if (row < column.size()) {
                std::cout << std::setw(10) << std::fixed << std::setprecision(4) << column[row] << " | ";
            } else {
                std::cout << std::setw(10) << "NA" << " | ";
            }
        } else if (TYPEOF(df[col]) == STRSXP) {
            Rcpp::StringVector column = df[col];
            if (row < column.size()) {
                std::cout << std::setw(10) << column[row] << " | ";
            } else {
                std::cout << std::setw(10) << "NA" << " | ";
            }
        }
    }
    std::cout << std::endl;
}

// Function to compare DataFrames
void compareDataFrames(Rcpp::DataFrame df1, Rcpp::DataFrame df2, const std::string& df1_name, const std::string& df2_name) {
    std::cout << "\n========== Comparing " << df1_name << " with " << df2_name << " ==========\n" << std::endl;
    
    // Print column names
    std::cout << "Column names in " << df1_name << ": ";
    Rcpp::StringVector names1 = df1.names();
    for (int i = 0; i < names1.size(); i++) {
        std::cout << names1[i] << " ";
    }
    std::cout << std::endl;
    
    std::cout << "Column names in " << df2_name << ": ";
    Rcpp::StringVector names2 = df2.names();
    for (int i = 0; i < names2.size(); i++) {
        std::cout << names2[i] << " ";
    }
    std::cout << std::endl;
    
    // Find the row count in each DataFrame
    int rowCount1 = 0;
    if (df1.size() > 0) {
        rowCount1 = Rcpp::as<Rcpp::NumericVector>(df1[0]).size();
    }
    
    int rowCount2 = 0;
    if (df2.size() > 0) {
        rowCount2 = Rcpp::as<Rcpp::NumericVector>(df2[0]).size();
    }
    
    std::cout << df1_name << " has " << rowCount1 << " rows." << std::endl;
    std::cout << df2_name << " has " << rowCount2 << " rows." << std::endl;
    
    // Compare row by row up to the smaller of the two DataFrames
    int min_rows = std::min(rowCount1, rowCount2);
    std::cout << "\nComparing the first " << min_rows << " rows:" << std::endl;
    
    for (int i = 0; i < min_rows; i++) {
        std::cout << "Row " << i << ":" << std::endl;
        printDataFrameRow(df1, i, df1_name);
        printDataFrameRow(df2, i, df2_name);
        std::cout << "----------------------------" << std::endl;
    }
    
    // Note if there are additional rows in either DataFrame
    if (rowCount1 > rowCount2) {
        std::cout << df1_name << " has " << (rowCount1 - rowCount2) << " additional rows." << std::endl;
    } else if (rowCount2 > rowCount1) {
        std::cout << df2_name << " has " << (rowCount2 - rowCount1) << " additional rows." << std::endl;
    }
}

// Function to create real-world test data
void createTestData(Rcpp::IntegerVector& indices, Rcpp::NumericVector& times, Rcpp::NumericVector& prices) {
    // Create a more complex pattern dataset
    const int dataSize = 200;
    times = Rcpp::NumericVector(dataSize);
    prices = Rcpp::NumericVector(dataSize);
    
    // Generate timestamps
    for (int i = 0; i < dataSize; i++) {
        times[i] = i + 1;
    }
    
    // Generate prices with multiple patterns
    // Base uptrend
    for (int i = 0; i < dataSize; i++) {
        prices[i] = 100 + i * 0.5 + 10 * sin(i * 0.1);
    }
    
    // Add an SHS pattern around index 50
    prices[50] = 125;
    prices[51] = 130;
    prices[52] = 135;
    prices[53] = 130;
    prices[54] = 125;
    prices[55] = 123;
    prices[56] = 128;
    prices[57] = 138;
    prices[58] = 128;
    prices[59] = 123;
    prices[60] = 126;
    prices[61] = 132;
    prices[62] = 136;
    prices[63] = 132;
    prices[64] = 126;
    
    // Breakout for SHS pattern
    for (int i = 65; i < 80; i++) {
        prices[i] = 126 - (i - 64) * 2;
    }
    
    // Add an iSHS pattern around index 100
    prices[100] = 170;
    prices[101] = 165;
    prices[102] = 160;
    prices[103] = 165;
    prices[104] = 170;
    prices[105] = 172;
    prices[106] = 167;
    prices[107] = 157;
    prices[108] = 167;
    prices[109] = 172;
    prices[110] = 169;
    prices[111] = 163;
    prices[112] = 158;
    prices[113] = 163;
    prices[114] = 169;
    
    // Breakout for iSHS pattern
    for (int i = 115; i < 130; i++) {
        prices[i] = 169 + (i - 114) * 2;
    }
    
    // Create pivot point indices (simplified for this example)
    std::vector<int> indicesVec;
    for (int i = 0; i < dataSize; i += 10) {
        indicesVec.push_back(i);
    }
    // Add specific pattern points
    indicesVec.push_back(50);
    indicesVec.push_back(52);
    indicesVec.push_back(54);
    indicesVec.push_back(57);
    indicesVec.push_back(60);
    indicesVec.push_back(62);
    indicesVec.push_back(64);
    indicesVec.push_back(100);
    indicesVec.push_back(102);
    indicesVec.push_back(104);
    indicesVec.push_back(107);
    indicesVec.push_back(110);
    indicesVec.push_back(112);
    indicesVec.push_back(114);
    
    // Sort and remove duplicates
    std::sort(indicesVec.begin(), indicesVec.end());
    indicesVec.erase(std::unique(indicesVec.begin(), indicesVec.end()), indicesVec.end());
    
    // Convert to Rcpp::IntegerVector
    indices = Rcpp::IntegerVector(indicesVec.begin(), indicesVec.end());
}

int main() {
    std::cout << "====== Pattern Detection Test Suite ======" << std::endl;
    std::cout << "This program compares different pattern detection implementations" << std::endl;
    std::cout << "===========================================" << std::endl;
    
    // Create test data
    Rcpp::IntegerVector indices;
    Rcpp::NumericVector times;
    Rcpp::NumericVector prices;
    createTestData(indices, times, prices);
    
    std::cout << "\nTest data created:" << std::endl;
    std::cout << "- Number of timestamps: " << times.size() << std::endl;
    std::cout << "- Number of price points: " << prices.size() << std::endl;
    std::cout << "- Number of pivot indices: " << indices.size() << std::endl;
    
    // Test data summary
    std::cout << "\nPivot indices: ";
    for (int i = 0; i < std::min(10, (int)indices.size()); i++) {
        std::cout << indices[i] << " ";
    }
    if (indices.size() > 10) std::cout << "...";
    std::cout << std::endl;
    
    try {
        std::cout << "\n===== Running findPatterns (OOP Implementation) =====" << std::endl;
        auto start1 = std::chrono::high_resolution_clock::now();
        Rcpp::List findPatterns_result = findPatterns(indices, times, prices);
        auto end1 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> elapsed1 = end1 - start1;
        std::cout << "findPatterns completed in " << elapsed1.count() << " ms" << std::endl;
        
        std::cout << "\n===== Running FastFindII (Procedural Implementation) =====" << std::endl;
        auto start2 = std::chrono::high_resolution_clock::now();
        Rcpp::DataFrame fastfind_result = FastFindII(indices, times, prices);
        auto end2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> elapsed2 = end2 - start2;
        std::cout << "FastFindII completed in " << elapsed2.count() << " ms" << std::endl;
        
        // Get the patternInfo DataFrame from the findPatterns result
        Rcpp::DataFrame patternInfo = Rcpp::as<Rcpp::DataFrame>(findPatterns_result["patternInfo"]);
        
        // Compare the results
        compareDataFrames(patternInfo, fastfind_result, "findPatterns", "FastFindII");
        
        std::cout << "\n===== Implementation Comparison Summary =====" << std::endl;
        std::cout << "OOP Implementation (findPatterns):" << std::endl;
        std::cout << "- Execution time: " << elapsed1.count() << " ms" << std::endl;
        std::cout << "- Pattern count: " << Rcpp::as<Rcpp::NumericVector>(patternInfo[0]).size() << std::endl;
        
        std::cout << "\nProcedural Implementation (FastFindII):" << std::endl;
        std::cout << "- Execution time: " << elapsed2.count() << " ms" << std::endl;
        std::cout << "- Pattern count: " << Rcpp::as<Rcpp::NumericVector>(fastfind_result[0]).size() << std::endl;
        
        std::cout << "\nPerformance difference: " << 
            (elapsed1.count() > elapsed2.count() ? 
                "FastFindII is " + std::to_string(elapsed1.count() / elapsed2.count()) + "x faster" :
                "findPatterns is " + std::to_string(elapsed2.count() / elapsed1.count()) + "x faster") 
            << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Exception caught: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Unknown exception caught" << std::endl;
        return 1;
    }
    
    std::cout << "\nTest completed successfully" << std::endl;
    return 0;
} 