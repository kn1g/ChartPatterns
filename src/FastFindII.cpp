#include <vector>
#include"cppHeader.hpp"

// define detailed logging macro for clarity - can be easily enabled/disabled
#define DETAILED_LOG true

//' @name FastFindII
//' @title Pattern Detection: SHS and iSHS
//' @description Detects Shoulder-Head-Shoulder (SHS) and inverted Shoulder-Head-Shoulder (iSHS) patterns
//'              in financial time series data. The algorithm analyzes pivot points to identify pattern 
//'              formations and validates them through breakout detection.
//' @param PrePro_indexFilter Vector of indices identifying pivot points in the original data series
//' @param Original_times Vector with timestamps or indices for each data point
//' @param Original_prices Vector with price values corresponding to each timestamp
//' @return Returns a list containing:
//'    - patternInfo: DataFrame with basic pattern identification and metrics
//'    - Features1to20: DataFrame with pattern shape features and measurements
//'    - Features21to40: DataFrame with trend analysis measurements
//' @details
//'   This implementation records both valid and invalid pattern candidates.
//'   For SHS patterns: Checks that first point is below the neckline
//'   For iSHS patterns: Checks that first point is above the neckline
//'   Breakout detection confirms pattern validity by checking price crossing the neckline
//' @examples
//' c(1:10)
//'
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame FastFindII(IntegerVector PrePro_indexFilter,
                           NumericVector Original_times,
                           NumericVector Original_prices
){
  
  // Verify index vector starts at zero
  if(PrePro_indexFilter[0] != 0){
    Function warning("warning");
    warning("PrePro_indexFilter indices do not start at zero.");
  }
  
  // Validate input: Ensure we have enough pivot points
  if (PrePro_indexFilter.size() < 7) {
    // Remove debug output
    // Instead of using non-existent createEmptyResults(), revert to original code
    // Create empty but valid DataFrame structures for all return values
    Rcpp::DataFrame emptyDF = Rcpp::DataFrame::create();
    return Rcpp::List::create(
      Rcpp::Named("patternInfo")  = emptyDF,
      Rcpp::Named("Features1to20")  = emptyDF,
      Rcpp::Named("Features21to40")  = emptyDF
    );
  }
  
  // Validate input: Check if any index is out of bounds
  for (int i = 0; i < PrePro_indexFilter.size(); ++i) {
    if (PrePro_indexFilter[i] < 0 || PrePro_indexFilter[i] >= Original_prices.size()) {
      // Instead of using non-existent createEmptyResults(), revert to original code
      Rcpp::DataFrame emptyDF = Rcpp::DataFrame::create();
      return Rcpp::List::create(
        Rcpp::Named("patternInfo")  = emptyDF,
        Rcpp::Named("Features1to20")  = emptyDF,
        Rcpp::Named("Features21to40")  = emptyDF
      );
    }
  }
  
  // Extract pivot points from the original dataset
  NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
  NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
  
  // Ensure we have enough data after filtering
  if (QuerySeries_prices.size() < 7) {
    // Instead of using non-existent createEmptyResults(), revert to original code
    Rcpp::DataFrame emptyDF = Rcpp::DataFrame::create();
    return Rcpp::List::create(
      Rcpp::Named("patternInfo")  = emptyDF,
      Rcpp::Named("Features1to20")  = emptyDF,
      Rcpp::Named("Features21to40")  = emptyDF
    );
  }
  
  // Variables for calculation
  double neckline_linearInterpolatedPointAtj;
  
  // Pattern information storage vectors
  std::vector<int> firstIndexPrePro;
  std::vector<int> firstindexOrigi;
  std::vector<int> breakoutIndex;
  std::vector<int> patternLength;
  std::vector<int> patternLengthInDays;
  std::vector<int> timeStamp;
  std::vector<int> priceStamp;
  std::vector<std::string> PatternName;
  std::vector<std::string> PatternGroup;
  std::vector<bool> trendPrediction;
  std::vector<bool> validPattern; // Add validation flag for pattern tracking
  
  // Pattern feature vectors
  std::vector<double> slopeNeckline;
  std::vector<double> slopePIP_1;
  std::vector<double> slopePIP_2;
  std::vector<double> slopePIP_3;
  std::vector<double> slopePIP_4;
  std::vector<double> slopePIP_5;
  std::vector<double> slopePIP_6;
  std::vector<double> slopePIP_7;
  std::vector<double> length_Neckline;
  std::vector<double> length_1;
  std::vector<double> length_2;
  std::vector<double> length_3;
  std::vector<double> length_4;
  std::vector<double> length_5;
  std::vector<double> length_6;
  std::vector<double> length_7;
  
  // Trend measurement vectors
  int AnzahlAufsteigenderTiefpunkteBeforeValue;
  std::vector<int> AnzahlAufsteigenderTiefpunkteBefore;      // Rising lows before pattern
  int AnzahlAufsteigenderHochpunkteBeforeValue;
  std::vector<int> AnzahlAufsteigenderHochpunkteBefore;      // Rising highs before pattern
  int AnzahlAbsteigenderHochpunkteAfterValue;
  std::vector<int> AnzahlAbsteigenderHochpunkteAfter;
  // int AnzahlAbsteigenderTiefpunkteAfterValue;
  // std::vector<int> AnzahlAbsteigenderTiefpunkteAfter;
  int AnzahlAbsteigenderTiefpunkteBeforeValue;
  std::vector<int> AnzahlAbsteigenderTiefpunkteBefore;       // Falling lows before pattern
  int AnzahlAbsteigenderHochpunkteBeforeValue;
  std::vector<int> AnzahlAbsteigenderHochpunkteBefore;
  // int AnzahlAufsteigenderHochpunkteAfterValue;
  // std::vector<int> AnzahlAufsteigenderHochpunkteAfter;
  int AnzahlAufsteigenderTiefpunkteAfterValue;
  std::vector<int> AnzahlAufsteigenderTiefpunkteAfter;       // Rising lows after pattern
  
  // Start pattern detection
  // Remove debug output
  // Rcpp::Rcout << "FastFindII: Starting pattern detection with " << PrePro_indexFilter.size() << " pivot points\n";
  
  // Safety check before entering the main loop
  if (QuerySeries_prices.size() <= 6) {
    // Remove debug output
    // Instead of using non-existent createEmptyResults(), revert to original code
    Rcpp::DataFrame emptyDF = Rcpp::DataFrame::create();
    return Rcpp::List::create(
      Rcpp::Named("patternInfo")  = emptyDF,
      Rcpp::Named("Features1to20")  = emptyDF,
      Rcpp::Named("Features21to40")  = emptyDF
    );
  }
  
  // main loop through all possible pattern starting points
  for(int i=0; i < QuerySeries_prices.size() - 6; ++i){
    
    // TWO SEPARATE PATTERN DETECTION BLOCKS - INDEPENDENTLY EXECUTED
    
    // =============== SHS PATTERN DETECTION ===============
    // Check if the points satisfy basic SHS (Shoulder-Head-Shoulder) conditions
    // The critical condition checks ensure proper shape and neckline relationships
    bool isSHSPattern = QuerySeries_prices[i] < QuerySeries_prices[i+1] &&     // First point below left shoulder  
                        QuerySeries_prices[i] < QuerySeries_prices[i+2] &&      // First point below left trough
                        QuerySeries_prices[i+1] < QuerySeries_prices[i+3] &&    // Left shoulder below head
                        QuerySeries_prices[i+5] < QuerySeries_prices[i+3] &&    // Right shoulder below head
                        // Neckline relationship checks:
                        QuerySeries_prices[i+5] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) &&  // Right shoulder above neckline 
                        QuerySeries_prices[i+1] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]) &&   // Left shoulder above neckline
                        // CRITICAL: Ensure first point is below neckline (prevents skewed patterns)
                        QuerySeries_prices[i] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i]);
    
    // DIAGNOSTIC: Log position and SHS pattern check
    // Rcpp::Rcout << "[FastFindII-SHS] Checking SHS at position " << i << ": ";
    // Log the pattern price points
    // for (int idx = 0; idx < 6; idx++) {
    //     Rcpp::Rcout << QuerySeries_prices[i+idx] << " ";
    // }
    // Rcpp::Rcout << std::endl;
    
    // Log neckline values
    double rightNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]);
    double leftNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]);
    double firstPointNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i]);
    
    // Log key relationships
    // Rcpp::Rcout << "[FastFindII-SHS] First point vs left shoulder: " 
    //          << QuerySeries_prices[i] << " < " << QuerySeries_prices[i+1] 
    //          << " = " << (QuerySeries_prices[i] < QuerySeries_prices[i+1]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] First point vs left trough: " 
    //          << QuerySeries_prices[i] << " < " << QuerySeries_prices[i+2] 
    //          << " = " << (QuerySeries_prices[i] < QuerySeries_prices[i+2]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] Left shoulder vs head: " 
    //          << QuerySeries_prices[i+1] << " < " << QuerySeries_prices[i+3] 
    //          << " = " << (QuerySeries_prices[i+1] < QuerySeries_prices[i+3]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] Right shoulder vs head: " 
    //          << QuerySeries_prices[i+5] << " < " << QuerySeries_prices[i+3] 
    //          << " = " << (QuerySeries_prices[i+5] < QuerySeries_prices[i+3]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] Right shoulder vs neckline: " 
    //          << QuerySeries_prices[i+5] << " > " << rightNecklineValue 
    //          << " = " << (QuerySeries_prices[i+5] > rightNecklineValue) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] Left shoulder vs neckline: " 
    //          << QuerySeries_prices[i+1] << " > " << leftNecklineValue 
    //          << " = " << (QuerySeries_prices[i+1] > leftNecklineValue) << std::endl;
    // Rcpp::Rcout << "[FastFindII-SHS] First point vs neckline: " 
    //          << QuerySeries_prices[i] << " < " << firstPointNecklineValue 
    //          << " = " << (QuerySeries_prices[i] < firstPointNecklineValue) << std::endl;
    
    if(isSHSPattern) {
        // Rcpp::Rcout << "[FastFindII-SHS] Pattern detected at position " << i << std::endl;
        bool patternValid = false;  // Track validity of pattern

        // IMPROVED: Safer array bounds checking before breakout detection
        if (i+5 < PrePro_indexFilter.size() && PrePro_indexFilter[i+5] < Original_times.size() - 1) {
            // Proceed with breakout detection
      for(int j = PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
        
                // DIAGNOSTIC: Log breakout check position
                // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Checking position " << j 
                //          << ", distance from pattern: " << (j - PrePro_indexFilter[i+5]) << std::endl;
                
                // IMPROVED: More robust boundary check
                if(j < 0 || j >= Original_times.size() - 1) {
                    // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Index " << j << " or j+1 out of bounds (size: " 
                    //          << Original_times.size() << "), aborting" << std::endl;
                    break; // Exit if j or j+1 is out of bounds
                }
                
                // Pattern invalidation check: if price rises above right shoulder
                if(Original_prices[j] > QuerySeries_prices[i+5] && j != PrePro_indexFilter[i+5]){
                    // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Pattern invalidated - price (" 
                    //          << Original_prices[j] << ") rose above right shoulder (" 
                    //          << QuerySeries_prices[i+5] << ") at position " << j << std::endl;
                    break;
                }
                
                // Calculate interpolated neckline value at current position
                neckline_linearInterpolatedPointAtj = linearInterpolation(
                    QuerySeries_times[i+2], QuerySeries_times[i+4],
                    QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                    Original_times[j]
                );
                
                // Log the price vs neckline comparison
                // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Price: " << Original_prices[j] 
                //          << " vs Neckline: " << neckline_linearInterpolatedPointAtj 
                //          << " (Below neckline: " << (Original_prices[j] < neckline_linearInterpolatedPointAtj) << ")" << std::endl;
                
                // Check for breakout: price dropping below neckline
        if(Original_prices[j] < neckline_linearInterpolatedPointAtj){
          
                    // Log the next price check
                    // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Next price: " << Original_prices[j+1] 
                    //          << " vs Right shoulder: " << QuerySeries_prices[i+5] 
                    //          << " (Below shoulder: " << (Original_prices[j+1] < QuerySeries_prices[i+5]) << ")" << std::endl;
                    
                    // Valid breakout confirmation: next price still below right shoulder
          if(Original_prices[j+1] < QuerySeries_prices[i+5]){
                        // Pattern is valid!
                        patternValid = true;
                        
                        // Rcpp::Rcout << "[FastFindII-SHS-BREAKOUT] Valid breakout detected at position " << j 
                        //          << ", breakout price: " << Original_prices[j+1] << std::endl;
            
                        // Store pattern information
            firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
            firstIndexPrePro.push_back(i+1);
            breakoutIndex.push_back(j+1);
            PatternName.push_back("SHS");
            PatternGroup.push_back("SHS");
            patternLength.push_back(5);
                        validPattern.push_back(true); // This pattern is valid
                        
                        // Store evaluation information
                        timeStamp.push_back(Original_times[j+1]);     // Breakout timestamp
                        priceStamp.push_back(Original_prices[j+1]);   // Breakout price
                        patternLengthInDays.push_back(Original_times[j+1]-QuerySeries_times[i]); // Pattern duration

            // We want to know some more about the pattern we found (like shape, trends, ...)
            
            // To get information abouot the symetry of the pattern we save the length and the slope between the PIPs
            slopeNeckline.push_back(getSlope(QuerySeries_times[i+2],QuerySeries_times[i+4], QuerySeries_prices[i+2],   QuerySeries_prices[i+4]));
            slopePIP_1.push_back(getSlope(QuerySeries_times[i]  , QuerySeries_times[i+1], QuerySeries_prices[i],   QuerySeries_prices[i+1]));
            slopePIP_2.push_back(getSlope(QuerySeries_times[i+1], QuerySeries_times[i+2], QuerySeries_prices[i+1], QuerySeries_prices[i+2]));
            slopePIP_3.push_back(getSlope(QuerySeries_times[i+2], QuerySeries_times[i+3], QuerySeries_prices[i+2], QuerySeries_prices[i+3]));
            slopePIP_4.push_back(getSlope(QuerySeries_times[i+3], QuerySeries_times[i+4], QuerySeries_prices[i+3], QuerySeries_prices[i+4]));
            slopePIP_5.push_back(getSlope(QuerySeries_times[i+4], QuerySeries_times[i+5], QuerySeries_prices[i+4], QuerySeries_prices[i+5]));
            slopePIP_6.push_back(getSlope(QuerySeries_times[i+5], QuerySeries_times[i+6], QuerySeries_prices[i+5], QuerySeries_prices[i+6]));
            slopePIP_7.push_back(getSlope(QuerySeries_times[i+5], Original_times[j],   QuerySeries_prices[i+5], Original_prices[j]));
            length_Neckline.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+5]) ;
            length_1.push_back(QuerySeries_times[i+1]-QuerySeries_times[i]) ;
            length_2.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+1]) ;
            length_3.push_back(QuerySeries_times[i+3]-QuerySeries_times[i+2]) ;
            length_4.push_back(QuerySeries_times[i+4]-QuerySeries_times[i+3]) ;
            length_5.push_back(QuerySeries_times[i+5]-QuerySeries_times[i+4]) ;
            length_6.push_back(QuerySeries_times[i+6]-QuerySeries_times[i+5]) ;
            length_7.push_back(Original_times[j]-QuerySeries_times[i+6]) ;
            // We also want the information how long the preceeding and the following trend 
            // a trend is given by rising or falling highs and lows (the PIPs)
            // therefore we check how many highs/lows preceed the identified pattern
            
            // Aufsteigende Tiefpunkte zuvor
            AnzahlAufsteigenderTiefpunkteBeforeValue = 0;
            for(int rev = i; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                            AnzahlAufsteigenderTiefpunkteBeforeValue = AnzahlAufsteigenderTiefpunkteBeforeValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteBefore.push_back(AnzahlAufsteigenderTiefpunkteBeforeValue);
            
            // Abfallende Tiefpunkte zuvor
            AnzahlAbsteigenderTiefpunkteBeforeValue = 0;
            for(int rev = i; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                            AnzahlAbsteigenderTiefpunkteBeforeValue = AnzahlAbsteigenderTiefpunkteBeforeValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderTiefpunkteBefore.push_back(AnzahlAbsteigenderTiefpunkteBeforeValue);
            
            // Aufsteigende Hochpunkte zuvor
            AnzahlAufsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                            AnzahlAufsteigenderHochpunkteBeforeValue = AnzahlAufsteigenderHochpunkteBeforeValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderHochpunkteBefore.push_back(AnzahlAufsteigenderHochpunkteBeforeValue);
            
            // Abfallende Hochpunkte zuvor
            AnzahlAbsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                            AnzahlAbsteigenderHochpunkteBeforeValue = AnzahlAbsteigenderHochpunkteBeforeValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderHochpunkteBefore.push_back(AnzahlAbsteigenderHochpunkteBeforeValue);
            
            // Abfallende Hochpunkte danach
            AnzahlAbsteigenderHochpunkteAfterValue = 0;
            for(int forward = i+4; forward > 2; forward = forward+2){
              if(forward+2 < QuerySeries_prices.size() && // Ensure safe access to forward+2
                 QuerySeries_prices[forward] > QuerySeries_prices[forward+2]){
                            AnzahlAbsteigenderHochpunkteAfterValue = AnzahlAbsteigenderHochpunkteAfterValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderHochpunkteAfter.push_back(AnzahlAbsteigenderHochpunkteAfterValue);
            
                        // Aufsteigende Tiefpunkte danach
            AnzahlAufsteigenderTiefpunkteAfterValue = 0;
            for(int forward = i+4; forward > 2; forward = forward+2){
              if(forward+2 < QuerySeries_prices.size() && // Ensure safe access to forward+2
                 QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                            AnzahlAufsteigenderTiefpunkteAfterValue = AnzahlAufsteigenderTiefpunkteAfterValue + 1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteAfter.push_back(AnzahlAufsteigenderTiefpunkteAfterValue);
            
                        break; // Pattern found, no need to continue checking
                    }
                }
            } // End breakout detection
        }
        
        // If pattern wasn't valid but met initial conditions, record it as invalid
        if(!patternValid) {
            firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
            firstIndexPrePro.push_back(i+1);
            breakoutIndex.push_back(NA_INTEGER); // No breakout
            PatternName.push_back("SHS");
            PatternGroup.push_back("SHS");
            patternLength.push_back(5);
            validPattern.push_back(false); // This pattern is invalid
            
            // Add placeholder values for other vectors
            timeStamp.push_back(QuerySeries_times[i]);
            priceStamp.push_back(QuerySeries_prices[i]);
            patternLengthInDays.push_back(0);
            
            // Add placeholders for feature vectors
            slopeNeckline.push_back(getSlope(QuerySeries_times[i+2],QuerySeries_times[i+4], QuerySeries_prices[i+2], QuerySeries_prices[i+4]));
            slopePIP_1.push_back(getSlope(QuerySeries_times[i], QuerySeries_times[i+1], QuerySeries_prices[i], QuerySeries_prices[i+1]));
            slopePIP_2.push_back(getSlope(QuerySeries_times[i+1], QuerySeries_times[i+2], QuerySeries_prices[i+1], QuerySeries_prices[i+2]));
            slopePIP_3.push_back(getSlope(QuerySeries_times[i+2], QuerySeries_times[i+3], QuerySeries_prices[i+2], QuerySeries_prices[i+3]));
            slopePIP_4.push_back(getSlope(QuerySeries_times[i+3], QuerySeries_times[i+4], QuerySeries_prices[i+3], QuerySeries_prices[i+4]));
            slopePIP_5.push_back(getSlope(QuerySeries_times[i+4], QuerySeries_times[i+5], QuerySeries_prices[i+4], QuerySeries_prices[i+5]));
            slopePIP_6.push_back(0);
            slopePIP_7.push_back(0);
            length_Neckline.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+5]);
            length_1.push_back(QuerySeries_times[i+1]-QuerySeries_times[i]);
            length_2.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+1]);
            length_3.push_back(QuerySeries_times[i+3]-QuerySeries_times[i+2]);
            length_4.push_back(QuerySeries_times[i+4]-QuerySeries_times[i+3]);
            length_5.push_back(QuerySeries_times[i+5]-QuerySeries_times[i+4]);
            length_6.push_back(0);
            length_7.push_back(0);
            
            // Add placeholder trend values
            AnzahlAufsteigenderTiefpunkteBefore.push_back(0);
            AnzahlAufsteigenderHochpunkteBefore.push_back(0);
            AnzahlAbsteigenderTiefpunkteBefore.push_back(0);
            AnzahlAbsteigenderHochpunkteBefore.push_back(0);
            AnzahlAbsteigenderHochpunkteAfter.push_back(0);
            AnzahlAufsteigenderTiefpunkteAfter.push_back(0);
        }
    } // End SHS pattern check
    
    // =============== iSHS PATTERN DETECTION ===============
    // Check if the points satisfy basic iSHS (inverted Shoulder-Head-Shoulder) conditions
    // The pattern is essentially the mirror image of SHS, with critical neckline relationships
    bool isISHSPattern = QuerySeries_prices[i] > QuerySeries_prices[i+1] &&     // First point above left shoulder
                         QuerySeries_prices[i] > QuerySeries_prices[i+2] &&      // First point above left peak
                         QuerySeries_prices[i+1] > QuerySeries_prices[i+3] &&    // Left shoulder above head
                         QuerySeries_prices[i+5] > QuerySeries_prices[i+3] &&    // Right shoulder above head
                         // Neckline relationship checks:
                         QuerySeries_prices[i+5] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) &&  // Right shoulder below neckline 
                         QuerySeries_prices[i+1] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]) &&   // Left shoulder below neckline
                         // CRITICAL: Ensure first point is above neckline (prevents skewed patterns)
                         QuerySeries_prices[i] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i]);
    
    // DIAGNOSTIC: Log position and iSHS pattern check
    // Rcpp::Rcout << "[FastFindII-iSHS] Checking iSHS at position " << i << ": ";
    // Log the pattern price points
    // for (int idx = 0; idx < 6; idx++) {
    //     Rcpp::Rcout << QuerySeries_prices[i+idx] << " ";
    // }
    // Rcpp::Rcout << std::endl;
    
    // Log neckline values
    double iSHS_rightNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]);
    double iSHS_leftNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]);
    double iSHS_firstPointNecklineValue = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i]);
    
    // Log key relationships
    // Rcpp::Rcout << "[FastFindII-iSHS] First point vs left shoulder: " 
    //          << QuerySeries_prices[i] << " > " << QuerySeries_prices[i+1] 
    //          << " = " << (QuerySeries_prices[i] > QuerySeries_prices[i+1]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] First point vs left peak: " 
    //          << QuerySeries_prices[i] << " > " << QuerySeries_prices[i+2] 
    //          << " = " << (QuerySeries_prices[i] > QuerySeries_prices[i+2]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] Left shoulder vs head: " 
    //          << QuerySeries_prices[i+1] << " > " << QuerySeries_prices[i+3] 
    //          << " = " << (QuerySeries_prices[i+1] > QuerySeries_prices[i+3]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] Right shoulder vs head: " 
    //          << QuerySeries_prices[i+5] << " > " << QuerySeries_prices[i+3] 
    //          << " = " << (QuerySeries_prices[i+5] > QuerySeries_prices[i+3]) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] Right shoulder vs neckline: " 
    //          << QuerySeries_prices[i+5] << " < " << iSHS_rightNecklineValue 
    //          << " = " << (QuerySeries_prices[i+5] < iSHS_rightNecklineValue) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] Left shoulder vs neckline: " 
    //          << QuerySeries_prices[i+1] << " < " << iSHS_leftNecklineValue 
    //          << " = " << (QuerySeries_prices[i+1] < iSHS_leftNecklineValue) << std::endl;
    // Rcpp::Rcout << "[FastFindII-iSHS] First point vs neckline: " 
    //          << QuerySeries_prices[i] << " > " << iSHS_firstPointNecklineValue 
    //          << " = " << (QuerySeries_prices[i] > iSHS_firstPointNecklineValue) << std::endl;
    
    if(isISHSPattern) {
        // Rcpp::Rcout << "[FastFindII-iSHS] Pattern detected at position " << i << std::endl;
        bool patternValid = false;  // Track validity of pattern

        // IMPROVED: Safer array bounds checking before breakout detection
        if (i+5 < PrePro_indexFilter.size() && PrePro_indexFilter[i+5] < Original_times.size() - 1) {
            // Proceed with breakout detection
            for(int j = PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
                
                // DIAGNOSTIC: Log breakout check position
                // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Checking position " << j 
                //          << ", distance from pattern: " << (j - PrePro_indexFilter[i+5]) << std::endl;
                
                // IMPROVED: More robust boundary check
                if(j < 0 || j >= Original_times.size() - 1) {
                    // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Index " << j << " or j+1 out of bounds (size: " 
                    //          << Original_times.size() << "), aborting" << std::endl;
                    break; // Exit if j or j+1 is out of bounds
                }
                
                // Pattern invalidation check: if price falls below right shoulder
                if(Original_prices[j] < QuerySeries_prices[i+5] && j != PrePro_indexFilter[i+5]){
                    // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Pattern invalidated - price (" 
                    //          << Original_prices[j] << ") fell below right shoulder (" 
                    //          << QuerySeries_prices[i+5] << ") at position " << j << std::endl;
                break;
              }
                
                // Calculate interpolated neckline value at current position
                neckline_linearInterpolatedPointAtj = linearInterpolation(
                    QuerySeries_times[i+2], QuerySeries_times[i+4],
                    QuerySeries_prices[i+2], QuerySeries_prices[i+4],
                    Original_times[j]
                );
                
                // Log the price vs neckline comparison
                // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Price: " << Original_prices[j] 
                //          << " vs Neckline: " << neckline_linearInterpolatedPointAtj 
                //          << " (Above neckline: " << (Original_prices[j] > neckline_linearInterpolatedPointAtj) << ")" << std::endl;
                
                // Check for breakout: price rising above neckline
                if(Original_prices[j] > neckline_linearInterpolatedPointAtj){
                    
                    // Log the next price check
                    // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Next price: " << Original_prices[j+1] 
                    //          << " vs Right shoulder: " << QuerySeries_prices[i+5] 
                    //          << " (Above shoulder: " << (Original_prices[j+1] > QuerySeries_prices[i+5]) << ")" << std::endl;
                    
                    // Valid breakout confirmation: next price still above right shoulder
                    if(Original_prices[j+1] > QuerySeries_prices[i+5]){
                        // Pattern is valid!
                        patternValid = true;
                        
                        // Rcpp::Rcout << "[FastFindII-iSHS-BREAKOUT] Valid breakout detected at position " << j 
                        //          << ", breakout price: " << Original_prices[j+1] << std::endl;
                        
                        // Store pattern information
                        firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
                        firstIndexPrePro.push_back(i+1);
                        breakoutIndex.push_back(j+1);
                        PatternName.push_back("iSHS");
                        PatternGroup.push_back("SHS");
                        patternLength.push_back(5);
                        validPattern.push_back(true); // This pattern is valid

                        // Store evaluation information
                        timeStamp.push_back(Original_times[j+1]);     // Breakout timestamp
                        priceStamp.push_back(Original_prices[j+1]);   // Breakout price
                        patternLengthInDays.push_back(Original_times[j+1]-QuerySeries_times[i]); // Pattern duration
                        //------------------------
                        
                        // Calculate slopes and lengths
                        slopeNeckline.push_back(getSlope(QuerySeries_times[i+2],QuerySeries_times[i+4], QuerySeries_prices[i+2], QuerySeries_prices[i+4]));
                        slopePIP_1.push_back(getSlope(QuerySeries_times[i], QuerySeries_times[i+1], QuerySeries_prices[i], QuerySeries_prices[i+1]));
                        slopePIP_2.push_back(getSlope(QuerySeries_times[i+1], QuerySeries_times[i+2], QuerySeries_prices[i+1], QuerySeries_prices[i+2]));
                        slopePIP_3.push_back(getSlope(QuerySeries_times[i+2], QuerySeries_times[i+3], QuerySeries_prices[i+2], QuerySeries_prices[i+3]));
                        slopePIP_4.push_back(getSlope(QuerySeries_times[i+3], QuerySeries_times[i+4], QuerySeries_prices[i+3], QuerySeries_prices[i+4]));
                        slopePIP_5.push_back(getSlope(QuerySeries_times[i+4], QuerySeries_times[i+5], QuerySeries_prices[i+4], QuerySeries_prices[i+5]));
                        slopePIP_6.push_back(getSlope(QuerySeries_times[i+5], QuerySeries_times[i+6], QuerySeries_prices[i+5], QuerySeries_prices[i+6]));
                        slopePIP_7.push_back(getSlope(QuerySeries_times[i+5], Original_times[j], QuerySeries_prices[i+5], Original_prices[j]));
                        
                        length_Neckline.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+5]);
                        length_1.push_back(QuerySeries_times[i+1]-QuerySeries_times[i]);
                        length_2.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+1]);
                        length_3.push_back(QuerySeries_times[i+3]-QuerySeries_times[i+2]);
                        length_4.push_back(QuerySeries_times[i+4]-QuerySeries_times[i+3]);
                        length_5.push_back(QuerySeries_times[i+5]-QuerySeries_times[i+4]);
                        length_6.push_back(QuerySeries_times[i+6]-QuerySeries_times[i+5]);
                        length_7.push_back(Original_times[j]-QuerySeries_times[i+6]);
                        
                        // FIXED: Calculate trend measurements for iSHS patterns
                        // For iSHS, check for FALLING highs BEFORE pattern (bearish trend before bullish reversal)
            AnzahlAbsteigenderHochpunkteBeforeValue = 0;
                        for(int rev = i; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                            AnzahlAbsteigenderHochpunkteBeforeValue += 1;
                          } else {
                break;
              }
                        }
            AnzahlAbsteigenderHochpunkteBefore.push_back(AnzahlAbsteigenderHochpunkteBeforeValue);
            
                        // Other trend metrics for completeness (not primary for iSHS)
                        AnzahlAufsteigenderTiefpunkteBefore.push_back(0); 
                        AnzahlAufsteigenderHochpunkteBefore.push_back(0); 
                        AnzahlAbsteigenderTiefpunkteBefore.push_back(0); 
                        
                        // FIXED: For iSHS, check for RISING lows AFTER pattern (bullish trend confirmation)
            AnzahlAufsteigenderTiefpunkteAfterValue = 0;
                        for(int forward = i+5; forward < (QuerySeries_prices.size()-2); forward = forward+2){
                          if(forward+2 < QuerySeries_prices.size() && 
                 QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                            AnzahlAufsteigenderTiefpunkteAfterValue += 1;
                          } else {
                break;
              }
                        }
            AnzahlAufsteigenderTiefpunkteAfter.push_back(AnzahlAufsteigenderTiefpunkteAfterValue);
                        
                        // Not as relevant for iSHS pattern
                        AnzahlAbsteigenderHochpunkteAfter.push_back(0);
                        
                        break; // Pattern found, no need to continue checking
                    }
                }
            } // End breakout detection
        }
        
        // If pattern wasn't valid but met initial conditions, record it as invalid
        if(!patternValid) {
            firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
            firstIndexPrePro.push_back(i+1);
            breakoutIndex.push_back(NA_INTEGER); // No breakout
            PatternName.push_back("iSHS");
            PatternGroup.push_back("SHS");
            patternLength.push_back(5);
            validPattern.push_back(false); // This pattern is invalid
            
            // Add placeholder values for other vectors
            timeStamp.push_back(QuerySeries_times[i]);
            priceStamp.push_back(QuerySeries_prices[i]);
            patternLengthInDays.push_back(0);
            
            // Add placeholders for feature vectors
            slopeNeckline.push_back(getSlope(QuerySeries_times[i+2],QuerySeries_times[i+4], QuerySeries_prices[i+2], QuerySeries_prices[i+4]));
            slopePIP_1.push_back(getSlope(QuerySeries_times[i], QuerySeries_times[i+1], QuerySeries_prices[i], QuerySeries_prices[i+1]));
            slopePIP_2.push_back(getSlope(QuerySeries_times[i+1], QuerySeries_times[i+2], QuerySeries_prices[i+1], QuerySeries_prices[i+2]));
            slopePIP_3.push_back(getSlope(QuerySeries_times[i+2], QuerySeries_times[i+3], QuerySeries_prices[i+2], QuerySeries_prices[i+3]));
            slopePIP_4.push_back(getSlope(QuerySeries_times[i+3], QuerySeries_times[i+4], QuerySeries_prices[i+3], QuerySeries_prices[i+4]));
            slopePIP_5.push_back(getSlope(QuerySeries_times[i+4], QuerySeries_times[i+5], QuerySeries_prices[i+4], QuerySeries_prices[i+5]));
            slopePIP_6.push_back(0);
            slopePIP_7.push_back(0);
            length_Neckline.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+5]);
            length_1.push_back(QuerySeries_times[i+1]-QuerySeries_times[i]);
            length_2.push_back(QuerySeries_times[i+2]-QuerySeries_times[i+1]);
            length_3.push_back(QuerySeries_times[i+3]-QuerySeries_times[i+2]);
            length_4.push_back(QuerySeries_times[i+4]-QuerySeries_times[i+3]);
            length_5.push_back(QuerySeries_times[i+5]-QuerySeries_times[i+4]);
            length_6.push_back(0);
            length_7.push_back(0);
            
            // FIXED: Add placeholder trend values with proper focus for iSHS
            AnzahlAufsteigenderTiefpunkteBefore.push_back(0);
            AnzahlAufsteigenderHochpunkteBefore.push_back(0);
            AnzahlAbsteigenderTiefpunkteBefore.push_back(0);
            AnzahlAbsteigenderHochpunkteBefore.push_back(0);
            AnzahlAbsteigenderHochpunkteAfter.push_back(0);
            AnzahlAufsteigenderTiefpunkteAfter.push_back(0);
        }
    } // End iSHS pattern check
    
  } // End main for loop
  
  // IMPROVED: Enhanced debug information with pattern counts
  int validSHSCount = 0;
  int invalidSHSCount = 0;
  int validiSHSCount = 0;
  int invalidiSHSCount = 0;
  
  for (size_t i = 0; i < PatternName.size(); i++) {
    if (PatternName[i] == "SHS") {
      if (validPattern[i]) validSHSCount++; else invalidSHSCount++;
    } else if (PatternName[i] == "iSHS") {
      if (validPattern[i]) validiSHSCount++; else invalidiSHSCount++;
    }
  }
  
  // Remove result summary output
  /*
  Rcpp::Rcout << "=========== FastFindII Results Summary ===========" << std::endl;
  Rcpp::Rcout << "Total patterns detected: " << PatternName.size() << std::endl;
  Rcpp::Rcout << "  SHS patterns: " << (validSHSCount + invalidSHSCount) << " (Valid: " << validSHSCount 
            << ", Invalid: " << invalidSHSCount << ")" << std::endl;
  Rcpp::Rcout << "  iSHS patterns: " << (validiSHSCount + invalidiSHSCount) << " (Valid: " << validiSHSCount 
            << ", Invalid: " << invalidiSHSCount << ")" << std::endl;
  Rcpp::Rcout << "=================================================" << std::endl;
  */
  
  // Print detailed debug info about vector sizes if needed
  if (DETAILED_LOG) {
    // Rcpp::Rcout << "Vector sizes:" << std::endl;
    // Rcpp::Rcout << "PatternName: " << PatternName.size() << std::endl;
    // Rcpp::Rcout << "PatternGroup: " << PatternGroup.size() << std::endl;
    // Rcpp::Rcout << "patternLength: " << patternLength.size() << std::endl;
    // Rcpp::Rcout << "firstIndexPrePro: " << firstIndexPrePro.size() << std::endl;
    // Rcpp::Rcout << "firstindexOrigi: " << firstindexOrigi.size() << std::endl;
    // Rcpp::Rcout << "breakoutIndex: " << breakoutIndex.size() << std::endl;
    // Rcpp::Rcout << "timeStamp: " << timeStamp.size() << std::endl;
    // Rcpp::Rcout << "priceStamp: " << priceStamp.size() << std::endl;
    // Rcpp::Rcout << "patternLengthInDays: " << patternLengthInDays.size() << std::endl;
    // Feature vectors
    // Rcpp::Rcout << "slopeNeckline: " << slopeNeckline.size() << std::endl;
    // Rcpp::Rcout << "length_Neckline: " << length_Neckline.size() << std::endl;
    // Trend vectors
    // Rcpp::Rcout << "AnzahlAufsteigenderTiefpunkteBefore: " << AnzahlAufsteigenderTiefpunkteBefore.size() << std::endl;
    // Rcpp::Rcout << "AnzahlAufsteigenderTiefpunkteAfter: " << AnzahlAufsteigenderTiefpunkteAfter.size() << std::endl;
  }
  
  // R cannot handle data frames with more than 20 columns, so we split the information into multiple data frames
  // and return them as a list structure
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(
    Rcpp::Named("PatternName")              = PatternName,        // Type of pattern (SHS or iSHS)
    Rcpp::Named("PatternGroup")             = PatternGroup,       // Pattern family grouping
    Rcpp::Named("validPattern")             = validPattern,       // Whether pattern has a valid breakout
    Rcpp::Named("patternLength")            = patternLength,      // Number of pattern points
    Rcpp::Named("firstIndexinPrePro")       = firstIndexPrePro,   // Starting index in pivot point series
    Rcpp::Named("firstIndexinOriginal")     = firstindexOrigi,    // Starting index in original data
    Rcpp::Named("breakoutIndexinOrig")      = breakoutIndex,      // Breakout index in original data
    Rcpp::Named("TimeStamp")                = timeStamp,          // Timestamp at breakout
    Rcpp::Named("PriceStamp")               = priceStamp,         // Price at breakout
    Rcpp::Named("patternLengthInDays")      = patternLengthInDays // Duration of pattern in time units
  );
  
  // Shape features of detected patterns
  Rcpp::DataFrame Features1to20 = Rcpp::DataFrame::create(
    Rcpp::Named("f01")  = slopePIP_1,                // Slope between first point and left shoulder
    Rcpp::Named("f02")  = slopePIP_2,                // Slope between left shoulder and left trough/peak
    Rcpp::Named("f03")  = slopePIP_3,                // Slope between left trough/peak and head
    Rcpp::Named("f04")  = slopePIP_4,                // Slope between head and right trough/peak
    Rcpp::Named("f05")  = slopePIP_5,                // Slope between right trough/peak and right shoulder
    Rcpp::Named("f06")  = slopePIP_6,                // Slope after right shoulder
    Rcpp::Named("f07")  = slopePIP_7,                // Slope to breakout
    Rcpp::Named("f08")  = length_1,                  // Length between first point and left shoulder
    Rcpp::Named("f09")  = length_2,                  // Length between left shoulder and left trough/peak
    Rcpp::Named("f10")  = length_3,                  // Length between left trough/peak and head
    Rcpp::Named("f11")  = length_4,                  // Length between head and right trough/peak
    Rcpp::Named("f12")  = length_5,                  // Length between right trough/peak and right shoulder
    Rcpp::Named("f13")  = length_6,                  // Length after right shoulder
    Rcpp::Named("f14")  = length_7,                  // Length to breakout
    Rcpp::Named("slopeNackenlinie")  = slopeNeckline,// Slope of the neckline
    Rcpp::Named("lengthNackenlinie")  = length_Neckline // Length of the neckline
  );
  
  // Trend metrics around the pattern
  Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
    Rcpp::Named("AnzahlAufsteigenderTiefpunkteBefore") = AnzahlAufsteigenderTiefpunkteBefore,
    Rcpp::Named("AnzahlAufsteigenderHochpunkteBefore") = AnzahlAufsteigenderHochpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderTiefpunkteBefore")  = AnzahlAbsteigenderTiefpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderHochpunkteBefore")  = AnzahlAbsteigenderHochpunkteBefore,
    Rcpp::Named("AnzahlAbsteigenderHochpunkteAfter")   = AnzahlAbsteigenderHochpunkteAfter,
    // Rcpp::Named("AnzahlAbsteigenderTiefpunkteAfter")   = AnzahlAbsteigenderTiefpunkteAfter
    Rcpp::Named("AnzahlAufsteigenderTiefpunkteAfter")  = AnzahlAufsteigenderTiefpunkteAfter
    // Rcpp::Named("AnzahlAufsteigenderHochpunkteAfter")  = AnzahlAufsteigenderHochpunkteAfter
  
  );
  
  
  return Rcpp::List::create(
    Rcpp::Named("patternInfo")  = patternInfo,     // Basic pattern information
    Rcpp::Named("Features1to20")  = Features1to20, // Pattern shape features
    Rcpp::Named("Features21to40")  = Features21to41 // Trend analysis metrics
  );
}
