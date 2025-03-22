#include <vector>
#include <cmath>
#include"cppHeader.hpp"



//' @name fastFind
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
 Rcpp::DataFrame fastFind_chaosRegin(IntegerVector PrePro_indexFilter,
                          NumericVector Original_times,
                          NumericVector Original_prices
 ){
   
   // Sucht PIPs im Originaldatenstz
   NumericVector QuerySeries_times  = Original_times[PrePro_indexFilter];
   NumericVector QuerySeries_prices = Original_prices[PrePro_indexFilter];
   
   // Initiierung einiger Rechenvariablen
   double neckline_linearInterpolatedPointAtj;
   
   // Valid pattern
   std::vector<bool> validPattern;
   
   // Variablen zum abspeichern der Musterinformationen
   // Indieces
   std::vector<int> firstIndexPrePro;
   std::vector<int> firstindexOrigi;
   std::vector<int> breakoutIndex;
   // Timestamps (x)
   std::vector<int> timeStamp0;
   std::vector<int> timeStamp1;
   std::vector<int> timeStamp2;
   std::vector<int> timeStamp3;
   std::vector<int> timeStamp4;
   std::vector<int> timeStamp5;
   std::vector<int> timeStampBreakOut;
   // Pricestamps (y)
   std::vector<double> priceStamp0;
   std::vector<double> priceStamp1;
   std::vector<double> priceStamp2;
   std::vector<double> priceStamp3;
   std::vector<double> priceStamp4;
   std::vector<double> priceStamp5;
   std::vector<double> priceStampBreakOut;
   
   std::vector<std::string> PatternName;
   
   // Trendmessung
   // Price and Times of Trend Start and Trend end
   double TrendBeginnPreisValue;
   std::vector<double> TrendBeginnPreis;  
   int TrendBeginnZeitValue;
   std::vector<int> TrendBeginnZeit;  
   
   double TrendEndePreisValue;
   std::vector<double> TrendEndePreis;  
   int TrendEndeZeitValue;
   std::vector<int> TrendEndeZeit;  
   
   // Rendite und Trend Kennzahlen
   std::vector<double> Rendite1V;
   std::vector<double> Rendite3V;
   std::vector<double> Rendite5V;
   std::vector<double> Rendite10V;
   std::vector<double> Rendite30V;
   std::vector<double> Rendite60V;
   
   std::vector<double> relRendite13V;
   std::vector<double> relRendite12V;
   std::vector<double> relRendite1V;
   std::vector<double> relRendite2V;
   std::vector<double> relRendite4V;
   
   
   // main loop thorughFindShoulderHeadShoulder
   for(int i=0; i < (QuerySeries_prices.size() - 6); ++i){
     
     // SHS detection
     // This part checks the positions of the 7 Points
     if(QuerySeries_prices[i  ] < QuerySeries_prices[i+1] && // ensures that it is a low (this is done before)
        QuerySeries_prices[i  ] < QuerySeries_prices[i+2] && // patterndefinitions from here on
        QuerySeries_prices[i+1] < QuerySeries_prices[i+3] &&
        QuerySeries_prices[i+5] < QuerySeries_prices[i+3] &&
        // Check if Shoulders are above Neckline
        QuerySeries_prices[i+5] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) && // Right Shoulder above Neckline
        QuerySeries_prices[i+1] > linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]) &&  // Left Shoulder above Neckline
        // check that the first price is below neckline (else way too skewed)
        QuerySeries_prices[i  ] < linearInterpolation(QuerySeries_times[i+2], QuerySeries_times[i+4], QuerySeries_prices[i+2], QuerySeries_prices[i+4], QuerySeries_times[i])
     ){
       // Prepare default values for all fields that may not get updated
       bool localValid = false;
       int localBreakoutIndex = NA_INTEGER;
       int localTimeBO = NA_INTEGER;
       double localPriceBO = NA_REAL;
       
       double localBeginnPreis = -1;
       int localBeginnZeit = 99999991;
       double localEndePreis = -1;
       int localEndeZeit = 99999991;
       
       double Rendite1 = NA_REAL;
       double Rendite3 = NA_REAL;
       double Rendite5 = NA_REAL;
       double Rendite10 = NA_REAL;
       double Rendite30 = NA_REAL;
       double Rendite60 = NA_REAL;
       
       double relRendite13 = NA_REAL;
       double relRendite12 = NA_REAL;
       double relRendite1 = NA_REAL;
       double relRendite2 = NA_REAL;
       double relRendite4 = NA_REAL;
       
       // loop over data to find when the neckline is crossed = breakout
       for(int j= PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
         
         // if the original Prices rise above the right shoulder we can stop. The Pattern would not be valid
         if(Original_prices[j] > QuerySeries_prices[i+5] &&
            j != PrePro_indexFilter[i+5]){
           break;
         }
         
         // Calculate interpolated neckline
         neckline_linearInterpolatedPointAtj = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4], Original_times[j]);
         // check if Neckline is crossed
         if(Original_prices[j] < neckline_linearInterpolatedPointAtj){
           
           // now the pattern is valid. But we only buy, if the next price (buyprice) is still above the right shoulder (limitbuy) 
           // check if buyprice is above right shoulder - no buy happens - only buy if under right shoulder
           if(Original_prices[j+1] < QuerySeries_prices[i+5]){
             // Pattern is valid
             localValid = true;
             localBreakoutIndex = j + 1; // +1 for R indexing
             localTimeBO = Original_times[j+1];
             localPriceBO = Original_prices[j+1];
             
             // We also want the information how long the preceeding and the following trend 
             // a trend is given by rising or falling highs and lows (the PIPs)
             // therefore we check how many highs/lows preceed the identified pattern
             
             // Wir schauen bei SHS nur auf die aufsteigende Tiefpunkte zuvor
             if(i > 2){
               for(int rev = i; rev > 2; rev = rev-2){
                 if(QuerySeries_prices[rev] > QuerySeries_prices[rev-2]){
                   localBeginnPreis = QuerySeries_prices[rev-2];
                   localBeginnZeit = QuerySeries_times[rev-2];
                 }else{
                   break;
                 }
               } // endfor
             }
             
             // Wir schauen bei SHS nur auf die abfallende Hochpunkte danach
             if(i+5 < (QuerySeries_prices.size()-2)){
               for(int forward = i+5; forward < (QuerySeries_prices.size()-2); forward = forward+2){
                 if(QuerySeries_prices[forward] > QuerySeries_prices[forward+2]){
                   localEndePreis = QuerySeries_prices[forward+2];
                   localEndeZeit = QuerySeries_times[forward+2];
                 }else{
                   break;
                 }
               } // endfor
             }
             
             // Gleich auch die Renditen berechnen
             if(j < Original_prices.size()-2){
               int patternLengthInDays = Original_times[j+1] - QuerySeries_times[i];
               
               int relDiff13 = patternLengthInDays/3;
               int relDiff12 = patternLengthInDays/2;
               int relDiff1 = patternLengthInDays;
               int relDiff2 = patternLengthInDays*2;
               int relDiff4 = patternLengthInDays*4;
               
               for(int forward = j+1; forward < (Original_prices.size()-2); ++forward){
                 // Differenz in Tage berechnen
                 int timeDiff = Original_times[forward] - Original_times[j+1];
                 if(timeDiff > 1 && R_IsNA(Rendite1)) { Rendite1 = Original_prices[forward]; }
                 if(timeDiff > 3 && R_IsNA(Rendite3)) { Rendite3 = Original_prices[forward]; }
                 if(timeDiff > 5 && R_IsNA(Rendite5)) { Rendite5 = Original_prices[forward]; }
                 if(timeDiff > 10 && R_IsNA(Rendite10)){ Rendite10 = Original_prices[forward]; }
                 if(timeDiff > 30 && R_IsNA(Rendite30)){ Rendite30 = Original_prices[forward]; }
                 if(timeDiff > 60 && R_IsNA(Rendite60)){ Rendite60 = Original_prices[forward]; }
                 
                 if(timeDiff > relDiff13 && R_IsNA(relRendite13)){ relRendite13 = Original_prices[forward]; }
                 if(timeDiff > relDiff12 && R_IsNA(relRendite12)){ relRendite12 = Original_prices[forward]; }
                 if(timeDiff > relDiff1 && R_IsNA(relRendite1)) { relRendite1 = Original_prices[forward]; }
                 if(timeDiff > relDiff2 && R_IsNA(relRendite2)) { relRendite2 = Original_prices[forward]; }
                 if(timeDiff > relDiff4 && R_IsNA(relRendite4)) { relRendite4 = Original_prices[forward]; }
                 
                 if(!R_IsNA(relRendite4) || !R_IsNA(Rendite60)){ break; }
               }
             }
             
             break;
           } //end if - pattern has been detected
         } // if neckline is crossed
       } // end for loop to check if neckline is crossed
       
       // Always record this pattern candidate with all values
       PatternName.push_back("SHS");
       validPattern.push_back(localValid);
       firstIndexPrePro.push_back(i+1); // +1 for R's 1-based indexing
       firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
       breakoutIndex.push_back(localBreakoutIndex);
       
       timeStamp0.push_back(QuerySeries_times[i]);
       timeStamp1.push_back(QuerySeries_times[i+1]);
       timeStamp2.push_back(QuerySeries_times[i+2]);
       timeStamp3.push_back(QuerySeries_times[i+3]);
       timeStamp4.push_back(QuerySeries_times[i+4]);
       timeStamp5.push_back(QuerySeries_times[i+5]);
       timeStampBreakOut.push_back(localTimeBO);
       
       priceStamp0.push_back(QuerySeries_prices[i]);
       priceStamp1.push_back(QuerySeries_prices[i+1]);
       priceStamp2.push_back(QuerySeries_prices[i+2]);
       priceStamp3.push_back(QuerySeries_prices[i+3]);
       priceStamp4.push_back(QuerySeries_prices[i+4]);
       priceStamp5.push_back(QuerySeries_prices[i+5]);
       priceStampBreakOut.push_back(localPriceBO);
       
       TrendBeginnPreis.push_back(localBeginnPreis);
       TrendBeginnZeit.push_back(localBeginnZeit);
       TrendEndePreis.push_back(localEndePreis);
       TrendEndeZeit.push_back(localEndeZeit);
       
       Rendite1V.push_back(Rendite1);
       Rendite3V.push_back(Rendite3);
       Rendite5V.push_back(Rendite5);
       Rendite10V.push_back(Rendite10);
       Rendite30V.push_back(Rendite30);
       Rendite60V.push_back(Rendite60);
       
       relRendite13V.push_back(relRendite13);
       relRendite12V.push_back(relRendite12);
       relRendite1V.push_back(relRendite1);
       relRendite2V.push_back(relRendite2);
       relRendite4V.push_back(relRendite4);
     } // end if points satisfy the rough condition
     
     
     
     // find iSHS
     // This part checks the positions of the 7 Points
     if(QuerySeries_prices[i  ] > QuerySeries_prices[i+1] && // ensures that it is a low (this is done before)
        QuerySeries_prices[i  ] > QuerySeries_prices[i+2] && // patterndefinitions from here on
        QuerySeries_prices[i+1] > QuerySeries_prices[i+3] &&
        QuerySeries_prices[i+5] > QuerySeries_prices[i+3] &&
        // Check if Shoulders are above Neckline
        QuerySeries_prices[i+5] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+5]) && // Right Shoulder above Neckline
        QuerySeries_prices[i+1] < linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4],QuerySeries_times[i+1]) &&   // Left Shoulder above Neckline
        // Frist potint above neckling (else way too skewed)
        QuerySeries_prices[i ] > linearInterpolation(QuerySeries_times[i+2], QuerySeries_times[i+4], QuerySeries_prices[i+2], QuerySeries_prices[i+4], QuerySeries_times[i ])
     ){
       // Prepare default values for all fields that may not get updated
       bool localValid = false;
       int localBreakoutIndex = NA_INTEGER;
       int localTimeBO = NA_INTEGER;
       double localPriceBO = NA_REAL;
       
       double localBeginnPreis = -1;
       int localBeginnZeit = 99999991;
       double localEndePreis = -1;
       int localEndeZeit = 99999991;
       
       double Rendite1 = NA_REAL;
       double Rendite3 = NA_REAL;
       double Rendite5 = NA_REAL;
       double Rendite10 = NA_REAL;
       double Rendite30 = NA_REAL;
       double Rendite60 = NA_REAL;
       
       double relRendite13 = NA_REAL;
       double relRendite12 = NA_REAL;
       double relRendite1 = NA_REAL;
       double relRendite2 = NA_REAL;
       double relRendite4 = NA_REAL;
       
       // loop over data to find when the neckline is crossed = breakout
       for(int j= PrePro_indexFilter[i+5]; j < Original_times.size()-1; ++j){
         
         if(Original_prices[j] < QuerySeries_prices[i+5] &&
            j != PrePro_indexFilter[i+5]){
           break;
         }
         
         // Calculate interpolated neckline
         neckline_linearInterpolatedPointAtj = linearInterpolation(QuerySeries_times[i+2],QuerySeries_times[i+4],QuerySeries_prices[i+2],QuerySeries_prices[i+4], Original_times[j]);
         // check if Neckline is crossed
         if(Original_prices[j] > neckline_linearInterpolatedPointAtj){
           
           // check if buyprice is under right shoulder - no buy happens - only buy if above right shoulder
           if(Original_prices[j+1] > QuerySeries_prices[i+5]){
             // Pattern is valid
             localValid = true;
             localBreakoutIndex = j + 1; // +1 for R indexing
             localTimeBO = Original_times[j+1];
             localPriceBO = Original_prices[j+1];
             
             // We also want the information how long the preceeding and the following trend 
             // a trend is given by rising or falling highs and lows (the PIPs)
             // therefore we check how many highs/lows preceed the identified pattern
             
             // Wir schauen bei iSHS nur auf die abfallende Hochpunkte davor
             if(i > 2){
               for(int rev = i; rev > 2; rev = rev-2){
                 if(QuerySeries_prices[rev] < QuerySeries_prices[rev-2]){
                   localBeginnPreis = QuerySeries_prices[rev-2];
                   localBeginnZeit = QuerySeries_times[rev-2];
                 }else{
                   break;
                 }
               } // endfor
             }
             
             // Wir schauen bei iSHS nur auf die aufsteignden Tiefpunkte danach
             if(i+5 < (QuerySeries_prices.size()-2)){
               for(int forward = i+5; forward < (QuerySeries_prices.size()-2); forward = forward+2){
                 if(QuerySeries_prices[forward] < QuerySeries_prices[forward+2]){
                   localEndePreis = QuerySeries_prices[forward+2];
                   localEndeZeit = QuerySeries_times[forward+2];
                 }else{
                   break;
                 }
               } // endfor
             }
             
             // Gleich auch die Renditen berechnen
             if(j < Original_prices.size()-2){
               int patternLengthInDays = Original_times[j+1] - QuerySeries_times[i];
               
               int relDiff13 = patternLengthInDays/3;
               int relDiff12 = patternLengthInDays/2;
               int relDiff1 = patternLengthInDays;
               int relDiff2 = patternLengthInDays*2;
               int relDiff4 = patternLengthInDays*4;
               
               for(int forward = j+1; forward < (Original_prices.size()-2); ++forward){
                 // Differenz in Tage berechnen
                 int timeDiff = Original_times[forward] - Original_times[j+1];
                 if(timeDiff > 1 && R_IsNA(Rendite1)) { Rendite1 = log(Original_prices[forward] / Original_prices[j+1]); }
                 if(timeDiff > 3 && R_IsNA(Rendite3)) { Rendite3 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > 5 && R_IsNA(Rendite5)) { Rendite5 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > 10 && R_IsNA(Rendite10)){ Rendite10 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > 30 && R_IsNA(Rendite30)){ Rendite30 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > 60 && R_IsNA(Rendite60)){ Rendite60 = Original_prices[forward] / Original_prices[j+1]; }
                 
                 if(timeDiff > relDiff13 && R_IsNA(relRendite13)){ relRendite13 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > relDiff12 && R_IsNA(relRendite12)){ relRendite12 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > relDiff1 && R_IsNA(relRendite1)) { relRendite1 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > relDiff2 && R_IsNA(relRendite2)) { relRendite2 = Original_prices[forward] / Original_prices[j+1]; }
                 if(timeDiff > relDiff4 && R_IsNA(relRendite4)) { relRendite4 = Original_prices[forward] / Original_prices[j+1]; }
                 
                 if(!R_IsNA(relRendite4) || !R_IsNA(Rendite60)){ break; }
               }
             }
             
             break;
           } // buy price shoulder check
           
         } // if neckline is crossed
       } // loop over data when neckline corssed 
       
       // Always record this pattern candidate with all values
       PatternName.push_back("iSHS");
       validPattern.push_back(localValid);
       firstIndexPrePro.push_back(i+1); // +1 for R's 1-based indexing
       firstindexOrigi.push_back(PrePro_indexFilter[i]+1);
       breakoutIndex.push_back(localBreakoutIndex);
       
       timeStamp0.push_back(QuerySeries_times[i]);
       timeStamp1.push_back(QuerySeries_times[i+1]);
       timeStamp2.push_back(QuerySeries_times[i+2]);
       timeStamp3.push_back(QuerySeries_times[i+3]);
       timeStamp4.push_back(QuerySeries_times[i+4]);
       timeStamp5.push_back(QuerySeries_times[i+5]);
       timeStampBreakOut.push_back(localTimeBO);
       
       priceStamp0.push_back(QuerySeries_prices[i]);
       priceStamp1.push_back(QuerySeries_prices[i+1]);
       priceStamp2.push_back(QuerySeries_prices[i+2]);
       priceStamp3.push_back(QuerySeries_prices[i+3]);
       priceStamp4.push_back(QuerySeries_prices[i+4]);
       priceStamp5.push_back(QuerySeries_prices[i+5]);
       priceStampBreakOut.push_back(localPriceBO);
       
       TrendBeginnPreis.push_back(localBeginnPreis);
       TrendBeginnZeit.push_back(localBeginnZeit);
       TrendEndePreis.push_back(localEndePreis);
       TrendEndeZeit.push_back(localEndeZeit);
       
       Rendite1V.push_back(Rendite1);
       Rendite3V.push_back(Rendite3);
       Rendite5V.push_back(Rendite5);
       Rendite10V.push_back(Rendite10);
       Rendite30V.push_back(Rendite30);
       Rendite60V.push_back(Rendite60);
       
       relRendite13V.push_back(relRendite13);
       relRendite12V.push_back(relRendite12);
       relRendite1V.push_back(relRendite1);
       relRendite2V.push_back(relRendite2);
       relRendite4V.push_back(relRendite4);
     } //end if - pattern has been detected 
   
   } // end main loop
   
   
   // RCPP can not handle data.frames with more  than 20 columns. We need to split the information and then put it together in a LIST in the end
   // We return a data.Frame object but put it in a list - this works somehow (kind of wired but perfect for our purpose)
   Rcpp::DataFrame patternInfo = Rcpp::DataFrame::create(Rcpp::Named("PatternName")              = PatternName,
                                                         Rcpp::Named("validPattern")             = validPattern,
                                                         Rcpp::Named("firstIndexinPrePro")       = firstIndexPrePro,
                                                         Rcpp::Named("firstIndexinOriginal")     = firstindexOrigi,
                                                         Rcpp::Named("breakoutIndexinOrig")      = breakoutIndex,
                                                         Rcpp::Named("TrendBeginnPreis")         = TrendBeginnPreis,
                                                         Rcpp::Named("TrendBeginnZeit")          = TrendBeginnZeit,
                                                         Rcpp::Named("TrendEndePreis")           = TrendEndePreis,
                                                         Rcpp::Named("TrendEndeZeit")            = TrendEndeZeit
   );
   
   Rcpp::DataFrame Features2   = Rcpp::DataFrame::create(   Rcpp::Named("timeStamp0")           = timeStamp0,
                                                            Rcpp::Named("timeStamp1")           = timeStamp1,
                                                            Rcpp::Named("timeStamp2")           = timeStamp2,
                                                            Rcpp::Named("timeStamp3")           = timeStamp3,
                                                            Rcpp::Named("timeStamp4")           = timeStamp4,
                                                            Rcpp::Named("timeStamp5")           = timeStamp5,
                                                            Rcpp::Named("timeStampBreakOut")    = timeStampBreakOut,
                                                            Rcpp::Named("priceStamp0")          = priceStamp0,
                                                            Rcpp::Named("priceStamp1")          = priceStamp1,
                                                            Rcpp::Named("priceStamp2")          = priceStamp2,
                                                            Rcpp::Named("priceStamp3")          = priceStamp3,
                                                            Rcpp::Named("priceStamp4")          = priceStamp4,
                                                            Rcpp::Named("priceStamp5")          = priceStamp5,
                                                            Rcpp::Named("priceStampBreakOut")   = priceStampBreakOut
   );
   
   Rcpp::DataFrame Features21to41 = Rcpp::DataFrame::create(
     Rcpp::Named("Rendite1V")     = Rendite1V,
     Rcpp::Named("Rendite3V")     = Rendite3V,
     Rcpp::Named("Rendite5V")     = Rendite5V,
     Rcpp::Named("Rendite10V")    = Rendite10V,
     Rcpp::Named("Rendite30V")    = Rendite30V,
     Rcpp::Named("Rendite60V")    = Rendite60V,
     Rcpp::Named("relRendite13V") = relRendite13V,
     Rcpp::Named("relRendite12V") = relRendite12V,
     Rcpp::Named("relRendite1V")  = relRendite1V,
     Rcpp::Named("relRendite2V")  = relRendite2V,
     Rcpp::Named("relRendite4V")  = relRendite4V
   );
   
   return Rcpp::List::create(Rcpp::Named("patternInfo")     = patternInfo,
                             Rcpp::Named("Features2")       = Features2,
                             Rcpp::Named("Features21to40")  = Features21to41
   );
   
 }

