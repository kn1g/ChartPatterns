#ifndef cppHeader_hpp
#define cppHeader_hpp

#include <Rcpp.h>
using namespace Rcpp;
double getSlope(double x1, double x2, double y1, double y2);
double linearInterpolation(double x1, double x2, double y1, double y2, double atPosition);

#endif
