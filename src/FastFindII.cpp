#include <vector>
#include"cppHeader.hpp"



//' @name fastFindII
//' @title fastFind Patterns
//' @description The pattern recognition is done for all paterns in one loop. The single functions loop per pattern over the dataset
//' @param prices Vector with prices
//' @param time Vector with time or indieces
//' @param mask with PIPs in the price-time vectors
//' @return Returns First the index where a pattern is located
//' @examples
//' c(1:10)
//'
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame FastFindII(IntegerVector PrePro_indexFilter,
                           NumericVector Original_times,
                           NumericVector Original_prices
){
  
  // Controls whether the index starts at zero
  if(PrePro_indexFilter[0] != 0){
    Function warning("warning");
    warning("PrePro Vector indices does not start at Zero.");
  }
  
  // Sucht PIPs im Originaldatenstz
  NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
  NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
  
  // Initiierung einiger Rechenvariablen
  double neckline_linearInterpolatedPointAtj;
  
  // Variablen zum abspeichern der Musterinformationen
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
  
  // features
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
  
  // Trendmessung
  int AnzahlAufsteigenderTiefpunkteBeforeValue;
  std::vector<int> AnzahlAufsteigenderTiefpunkteBefore;
  int AnzahlAufsteigenderHochpunkteBeforeValue;
  std::vector<int> AnzahlAufsteigenderHochpunkteBefore;
  int AnzahlAbsteigenderHochpunkteAfterValue;
  std::vector<int> AnzahlAbsteigenderHochpunkteAfter;
  // int AnzahlAbsteigenderTiefpunkteAfterValue;
  // std::vector<int> AnzahlAbsteigenderTiefpunkteAfter;
  int AnzahlAbsteigenderTiefpunkteBeforeValue;
  std::vector<int> AnzahlAbsteigenderTiefpunkteBefore;
  int AnzahlAbsteigenderHochpunkteBeforeValue;
  std::vector<int> AnzahlAbsteigenderHochpunkteBefore;
  // int AnzahlAufsteigenderHochpunkteAfterValue;
  // std::vector<int> AnzahlAufsteigenderHochpunkteAfter;
  int AnzahlAufsteigenderTiefpunkteAfterValue;
  std::vector<int> AnzahlAufsteigenderTiefpunkteAfter;
  
  // main loop thorughFindShoulderHeadShoulder
  for(int i=0; i < (QuerySeries_prices.size() - 6); ++i){
    
    // FIX 1: Ensure i+5 is within bounds for PrePro_indexFilter access
    if(i+5 >= PrePro_indexFilter.size()) {
      break; // Exit loop if we can't safely access i+5
    }
    
    // This part checks the positions of the 7 Points
    if(QuerySeries_prices[i  ] < QuerySeries_prices[i+1] && // ensures that it is a low (this is done before)
       QuerySeries_prices[i  ] < QuerySeries_prices[i+2] && // patterndefinitions from here on
       QuerySeries_prices[i+1] < QuerySeries_prices[i+3] &&
       QuerySeries_prices[i+5] < QuerySeries_prices[i+3] &&
       // Check if Shoulders are above Neckline
       QuerySeries_prices[i+5] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) && // Right Shoulder above Neckline
       QuerySeries_prices[i+1] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1])    // Left Shoulder above Neckline
    ){
      // FIX 2: Ensure PrePro_indexFilter[i+5] is valid before using it
      if(PrePro_indexFilter[i+5] >= Original_times.size() - 1) {
        continue; // Skip this pattern if we can't safely check for breakout
      }
      
      // loop over data to find when the neckline is crossed = breakout
      for(int j = PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
        
        // FIX 3: Verify j is valid for Original_times access
        if(j < 0 || j >= Original_times.size()) {
          break; // Exit loop if j is out of bounds
        }
        
        // Calculate interpolated neckline
        neckline_linearInterpolatedPointAtj = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4], Original_times[j]);
        // check if Neckline is crossed
        
        if(Original_prices[j] < neckline_linearInterpolatedPointAtj){
          
          // FIX 4: Ensure j+1 is within bounds of Original_prices
          if(j+1 >= Original_prices.size()) {
            break; // Exit loop if we can't safely check j+1
          }
          
          // now the pattern is valid. But we only buy, if the next price (buyprice) is still above the right shoulder (limitbuy) 
          // check if buyprice is above right shoulder - no buy happens - only buy if under right shoulder
          if(Original_prices[j+1] < QuerySeries_prices[i+5]){
            
            // The pattern is valid and the we would open a position
            // From here on we save step by step relevant information about the pattern
            // First we save the index where the pattern occured in the original time series (this is redundant (see next step) but convinient) - WE NEED TO ADD 1 BECAUSE R INDIECES START AT 1 NOT 0 (like in C++)
            firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
            // second we save the index where the pattern occured in the preprocessed series - WE NEED TO ADD 1 BECAUSE R INDIECES START AT 1 NOT 0 (like in C++)
            firstIndexPrePro.push_back(i+1);
            // third we save the breakout index - WE NEED TO ADD 1 BECAUSE R INDIECES START AT 1 NOT 0 (like in C++)
            // The index is from the original time series
            breakoutIndex.push_back(j+1);
            // the pattern's name
            PatternName.push_back("SHS");
            // and a pattern group identifier (used for plotting later)
            PatternGroup.push_back("SHS");
            // let's also save the pattern's length 
            patternLength.push_back(5);
            
            // This is added to make the evaluation simpler
            // The time of the pattern
            timeStamp.push_back(Original_times[j+1]);
            // The abs price of the pattern
            priceStamp.push_back(Original_prices[j+1]);
            // Length in days
            patternLengthInDays.push_back(Original_times[j+1]-QuerySeries_times[i]);
            //------------------------

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
                AnzahlAufsteigenderTiefpunkteBeforeValue = AnzahlAufsteigenderTiefpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteBefore.push_back(AnzahlAufsteigenderTiefpunkteBeforeValue);
            
            // Abfallende Tiefpunkte zuvor
            AnzahlAbsteigenderTiefpunkteBeforeValue = 0;
            for(int rev = i; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                AnzahlAbsteigenderTiefpunkteBeforeValue = AnzahlAbsteigenderTiefpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderTiefpunkteBefore.push_back(AnzahlAbsteigenderTiefpunkteBeforeValue);
            
            // Aufsteigende Hochpunkte zuvor
            AnzahlAufsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                AnzahlAufsteigenderHochpunkteBeforeValue = AnzahlAufsteigenderHochpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderHochpunkteBefore.push_back(AnzahlAufsteigenderHochpunkteBeforeValue);
            
            // Abfallende Hochpunkte zuvor
            AnzahlAbsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                AnzahlAbsteigenderHochpunkteBeforeValue = AnzahlAbsteigenderHochpunkteBeforeValue +1;
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
                AnzahlAbsteigenderHochpunkteAfterValue = AnzahlAbsteigenderHochpunkteAfterValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderHochpunkteAfter.push_back(AnzahlAbsteigenderHochpunkteAfterValue);
            
            // Aufsteigende Hochpunkte danach
            AnzahlAufsteigenderTiefpunkteAfterValue = 0;
            for(int forward = i+4; forward > 2; forward = forward+2){
              if(forward+2 < QuerySeries_prices.size() && // Ensure safe access to forward+2
                 QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                AnzahlAufsteigenderTiefpunkteAfterValue = AnzahlAufsteigenderTiefpunkteAfterValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteAfter.push_back(AnzahlAufsteigenderTiefpunkteAfterValue);
            
            
            break;
          } //end if - pattern has been detected
        } // if neckline is crossed
        
        // FIX 7: Add j != PrePro_indexFilter[i+5] check to match FastFind_ChaosRegin.cpp
        // if the original Prices rise above the right shoulder we can stop. The Pattern would not be valid
        if(Original_prices[j] > QuerySeries_prices[i+5] && j != PrePro_indexFilter[i+5]){
          break;
        }
      } // end for loop to check if neckline is crossed
      
    } // end if points satisfy the rough condition
    
    // FIX 8: The same boundary checks for iSHS pattern detection
    // This part checks the positions of the 7 Points
    if(QuerySeries_prices[i  ] > QuerySeries_prices[i+1] && // ensures that it is a low (this is done before)
       QuerySeries_prices[i  ] > QuerySeries_prices[i+2] && // patterndefinitions from here on
       QuerySeries_prices[i+1] > QuerySeries_prices[i+3] &&
       QuerySeries_prices[i+5] > QuerySeries_prices[i+3] &&
       // Check if Shoulders are above Neckline
       QuerySeries_prices[i+5] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) && // Right Shoulder above Neckline
       QuerySeries_prices[i+1] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1])    // Left Shoulder above Neckline
    ){
      // FIX 9: Same boundary check as in SHS case
      if(PrePro_indexFilter[i+5] >= Original_times.size() - 1) {
        continue; // Skip this pattern if we can't safely check for breakout
      }
      
      // loop over data to find when the neckline is crossed = breakout
      for(int j= PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
        
        // FIX 10: Verify j is valid for Original_times access, same as SHS case
        if(j < 0 || j >= Original_times.size()) {
          break; // Exit loop if j is out of bounds
        }
        
        // Calculate interpolated neckline
        neckline_linearInterpolatedPointAtj = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4], Original_times[j]);
        
        // check if Neckline is crossed
        if(Original_prices[j] > neckline_linearInterpolatedPointAtj){
          
          // FIX 11: Ensure j+1 is within bounds of Original_prices for iSHS
          if(j+1 >= Original_prices.size()) {
            break; // Exit loop if we can't safely check j+1
          }
          
          // check if buyprice is under right shoulder - no buy happens - only buy if above right shoulder // Groesser Zeichen geandert
          if(Original_prices[j+1] > QuerySeries_prices[i+5]){
            
            // save patterninfo
            firstIndexPrePro.push_back(i+1);
            firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
            breakoutIndex.push_back(j+1);
            PatternName.push_back("iSHS");
            PatternGroup.push_back("SHS");
            patternLength.push_back(5);

            // This is added to make the evaluation simpler
            // The time of the pattern
            timeStamp.push_back(Original_times[j+1]);
            // The abs price of the pattern
            priceStamp.push_back(Original_prices[j+1]);
            // Length in days
            patternLengthInDays.push_back(Original_times[j+1]-QuerySeries_times[i]);
            //------------------------
            
            // calculate slopes
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
                AnzahlAufsteigenderTiefpunkteBeforeValue = AnzahlAufsteigenderTiefpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteBefore.push_back(AnzahlAufsteigenderTiefpunkteBeforeValue);
            
            // Abfallende Tiefpunkte zuvor
            AnzahlAbsteigenderTiefpunkteBeforeValue = 0;
            for(int rev = i; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                AnzahlAbsteigenderTiefpunkteBeforeValue = AnzahlAbsteigenderTiefpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderTiefpunkteBefore.push_back(AnzahlAbsteigenderTiefpunkteBeforeValue);
            
            // Aufsteigende Hochpunkte zuvor
            AnzahlAufsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                AnzahlAufsteigenderHochpunkteBeforeValue = AnzahlAufsteigenderHochpunkteBeforeValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderHochpunkteBefore.push_back(AnzahlAufsteigenderHochpunkteBeforeValue);
            
            // Abfallende Hochpunkte zuvor
            AnzahlAbsteigenderHochpunkteBeforeValue = 0;
            for(int rev = i-1; rev > 2; rev= rev-2){
              if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                AnzahlAbsteigenderHochpunkteBeforeValue = AnzahlAbsteigenderHochpunkteBeforeValue +1;
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
                AnzahlAbsteigenderHochpunkteAfterValue = AnzahlAbsteigenderHochpunkteAfterValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAbsteigenderHochpunkteAfter.push_back(AnzahlAbsteigenderHochpunkteAfterValue);
            
            // Aufsteigende Hochpunkte danach
            AnzahlAufsteigenderTiefpunkteAfterValue = 0;
            for(int forward = i+4; forward > 2; forward = forward+2){
              if(forward+2 < QuerySeries_prices.size() && // Ensure safe access to forward+2
                 QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                AnzahlAufsteigenderTiefpunkteAfterValue = AnzahlAufsteigenderTiefpunkteAfterValue +1;
              }else{
                break;
              }
            } // endfor
            AnzahlAufsteigenderTiefpunkteAfter.push_back(AnzahlAufsteigenderTiefpunkteAfterValue);
            
            
            break;
          } //end if - pattern has been detected
        } // if neckline is crossed
        
        // FIX 12: Add j != PrePro_indexFilter[i+5] check to match FastFind_ChaosRegin.cpp
        // if the original Prices fall below the right shoulder we can stop. The Pattern would not be valid
        if(Original_prices[j] < QuerySeries_prices[i+5] && j != PrePro_indexFilter[i+5]){
          break;
        }
      } // end for loop to check if neckline is crossed
      
    } // end if points satisfy the rough condition
    
  } // end main for loop which iterates over all datapoints
  
  // print the variables to detect length mismatches
  Rcpp::Rcout << "PatternName: " << PatternName.size() << std::endl;
  Rcpp::Rcout << "PatternGroup: " << PatternGroup.size() << std::endl;
  Rcpp::Rcout << "patternLength: " << patternLength.size() << std::endl;
  Rcpp::Rcout << "firstIndexPrePro: " << firstIndexPrePro.size() << std::endl;
  Rcpp::Rcout << "firstindexOrigi: " << firstindexOrigi.size() << std::endl;
  Rcpp::Rcout << "breakoutIndex: " << breakoutIndex.size() << std::endl;
  Rcpp::Rcout << "timeStamp: " << timeStamp.size() << std::endl;
  Rcpp::Rcout << "priceStamp: " << priceStamp.size() << std::endl;
  Rcpp::Rcout << "patternLengthInDays: " << patternLengthInDays.size() << std::endl;
  Rcpp::Rcout << "slopeNeckline: " << slopeNeckline.size() << std::endl;
  Rcpp::Rcout << "slopePIP_1: " << slopePIP_1.size() << std::endl;
  Rcpp::Rcout << "slopePIP_2: " << slopePIP_2.size() << std::endl;
  Rcpp::Rcout << "slopePIP_3: " << slopePIP_3.size() << std::endl;
  Rcpp::Rcout << "slopePIP_4: " << slopePIP_4.size() << std::endl;
  Rcpp::Rcout << "slopePIP_5: " << slopePIP_5.size() << std::endl;
  Rcpp::Rcout << "slopePIP_6: " << slopePIP_6.size() << std::endl;
  Rcpp::Rcout << "slopePIP_7: " << slopePIP_7.size() << std::endl;
  Rcpp::Rcout << "length_Neckline: " << length_Neckline.size() << std::endl;
  Rcpp::Rcout << "length_1: " << length_1.size() << std::endl;
  Rcpp::Rcout << "length_2: " << length_2.size() << std::endl;
  Rcpp::Rcout << "length_3: " << length_3.size() << std::endl;
  Rcpp::Rcout << "length_4: " << length_4.size() << std::endl;
  Rcpp::Rcout << "length_5: " << length_5.size() << std::endl;
  Rcpp::Rcout << "length_6: " << length_6.size() << std::endl;
  Rcpp::Rcout << "length_7: " << length_7.size() << std::endl;
  Rcpp::Rcout << "AnzahlAufsteigenderTiefpunkteBefore: " << AnzahlAufsteigenderTiefpunkteBefore.size() << std::endl;
  Rcpp::Rcout << "AnzahlAufsteigenderHochpunkteBefore: " << AnzahlAufsteigenderHochpunkteBefore.size() << std::endl;
  Rcpp::Rcout << "AnzahlAbsteigenderTiefpunkteBefore: " << AnzahlAbsteigenderTiefpunkteBefore.size() << std::endl;
  Rcpp::Rcout << "AnzahlAbsteigenderHochpunkteBefore: " << AnzahlAbsteigenderHochpunkteBefore.size() << std::endl;
  Rcpp::Rcout << "AnzahlAbsteigenderHochpunkteAfter: " << AnzahlAbsteigenderHochpunkteAfter.size() << std::endl;
  Rcpp::Rcout << "AnzahlAufsteigenderTiefpunkteAfter: " << AnzahlAufsteigenderTiefpunkteAfter.size() << std::endl;
  // Rcpp::Rcout << "AnzahlAufsteigenderHochpunkteAfter: " << AnzahlAufsteigenderHochpunkteAfter.size() << std::endl;
  // Rcpp::Rcout << "AnzahlAbsteigenderTiefpunkteAfter: " << AnzahlAbsteigenderTiefpunkteAfter.size() << std::endl;

  
  
  
  
  // RCPP can not handle data.frames with more  than 20 columns. We need to split the information and then put it together in a LIST in the end
  // We return a data.Frame object but put it in a list - this works somehow (kind of wired but perfect for our purpose)
  Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(Rcpp::Named("PatternName")              = PatternName,
                                                        Rcpp::Named("PatternGroup")             = PatternGroup,
                                                        Rcpp::Named("patternLength")            = patternLength,
                                                        Rcpp::Named("firstIndexinPrePro")       = firstIndexPrePro,
                                                        Rcpp::Named("firstIndexinOriginal")     = firstindexOrigi,
                                                        Rcpp::Named("breakoutIndexinOrig")      = breakoutIndex,
                                                        Rcpp::Named("TimeStamp")                = timeStamp,
                                                        Rcpp::Named("PriceStamp")               = priceStamp,
                                                        Rcpp::Named("patternLengthInDays")      = patternLengthInDays
  );
  
  Rcpp::DataFrame Features1to20 = Rcpp::DataFrame::create(Rcpp::Named("f01")  = slopePIP_1,
                                                          Rcpp::Named("f02")  = slopePIP_2,
                                                          Rcpp::Named("f03")  = slopePIP_3,
                                                          Rcpp::Named("f04")  = slopePIP_4,
                                                          Rcpp::Named("f05")  = slopePIP_5,
                                                          Rcpp::Named("f06")  = slopePIP_6,
                                                          Rcpp::Named("f07")  = slopePIP_7,
                                                          Rcpp::Named("f08")  = length_1,
                                                          Rcpp::Named("f09")  = length_2,
                                                          Rcpp::Named("f10")  = length_3,
                                                          Rcpp::Named("f11")  = length_4,
                                                          Rcpp::Named("f12")  = length_5,
                                                          Rcpp::Named("f13")  = length_6,
                                                          Rcpp::Named("f14")  = length_7,
                                                          Rcpp::Named("slopeNackenlinie")  = slopeNeckline,
                                                          Rcpp::Named("lengthNackenlinie")  = length_Neckline
  );
  
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
  
  
  return Rcpp::List::create(Rcpp::Named("patternInfo")  = patternInfo,
                            Rcpp::Named("Features1to20")  = Features1to20,
                            Rcpp::Named("Features21to40")  = Features21to41
  );
}
