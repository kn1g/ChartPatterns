#include"cppHeader.hpp"

//' @name linearInterpolation
//' @title linearInterpolation
//' @description Twodimensional linearinterpolation for a specific point
//' @param xCoordinates Two x coordinates
//' @param yCoordinates Coresponding two y coordinates
//' @param atPosition The Point to which the interpolation shall be done
//' @return Returns the linear interpolated y-value for the specific point
//' @examples
//' c(1:10)
//'
//' @export
// [[Rcpp::export]]
double linearInterpolation(double x1, double x2, double y1, double y2, double atPosition) {

  // start + delta y / delta x_1 * delta x_2
  double out = y2 + getSlope(x1,x2,y1,y2) * (atPosition - x2);
  return out;

}



