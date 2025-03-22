#include"cppHeader.hpp"


//' @name getSlope
//' @title getSlope
//' @description Calculates the slopes between two points in 2Dimensions
//' @param xCoordinates Two x coordinates
//' @param yCoordinates Coresponding two y coordinates
//' @return Returns the slope
//' @examples
//' c(1:10)
//'
//' @export
// [[Rcpp::export]]
double getSlope(double x1, double x2, double y1, double y2) {
  return (y2 - y1) / (x2 - x1);
}



