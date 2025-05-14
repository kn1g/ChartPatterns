#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
Rcpp::List fastDetectSHSiSHS(
    IntegerVector PrePro_indexFilter,
    NumericVector Original_times,
    NumericVector Original_prices
) {
    int M = PrePro_indexFilter.size();
    int N = Original_times.size();
    // Raw pointers for speed
    const int*   idx = PrePro_indexFilter.begin();
    const double* T  = Original_times.begin();
    const double* P  = Original_prices.begin();
    const double* FT = Original_times.begin();
    const double* FP = Original_prices.begin();

    std::vector<std::string> PatternName;
    std::vector<bool> validPattern;
    std::vector<int> firstIndexPrePro;
    std::vector<int> firstIndexOriginal;
    std::vector<int> breakoutIndexinOrig;

    // Reserve to avoid reallocations
    int reserve_est = M / 6 + 1;
    PatternName.reserve(reserve_est);
    validPattern.reserve(reserve_est);
    firstIndexPrePro.reserve(reserve_est);
    firstIndexOriginal.reserve(reserve_est);
    breakoutIndexinOrig.reserve(reserve_est);

    for (int i = 0; i + 5 < M; ++i) {
        // pivot points 0..5
        double t0 = T[i],   p0 = P[i];
        double t1 = T[i+1], p1 = P[i+1];
        double t2 = T[i+2], p2 = P[i+2];
        double t3 = T[i+3], p3 = P[i+3];
        double t4 = T[i+4], p4 = P[i+4];
        double t5 = T[i+5], p5 = P[i+5];

        // compute neckline line: y = slope*x + intercept
        double slope     = (p4 - p2) / (t4 - t2);
        double intercept = p2 - slope * t2;

        // SHS shape test
        bool isSHS =
            p0 < p1 && p0 < p2 &&
            p1 < p3 && p5 < p3 &&
            p5 > slope * t5 + intercept &&
            p1 > slope * t1 + intercept &&
            p0 < slope * t0 + intercept;

        // iSHS shape test
        bool isISHS =
            p0 > p1 && p0 > p2 &&
            p1 > p3 && p5 > p3 &&
            p5 < slope * t5 + intercept &&
            p1 < slope * t1 + intercept &&
            p0 > slope * t0 + intercept;

        if (!isSHS && !isISHS) continue;  // no pattern here

        // scan raw series for breakout/invalidation
        int orig5 = idx[i+5];
        int bo = -1;
        for (int j = orig5 + 1; j + 1 < N; ++j) {
            double y = slope * FT[j] + intercept;
            // invalidation
            if (isSHS && FP[j] > p5) break;
            if (isISHS && FP[j] < p5) break;
            // breakout
            if ((isSHS  && FP[j] <  y && FP[j+1] <  p5) ||
                (isISHS && FP[j] >  y && FP[j+1] >  p5)) {
                bo = j + 1;
                break;
            }
        }

        PatternName.push_back(isSHS ? "SHS" : "iSHS");
        validPattern.push_back(bo >= 0);
        firstIndexPrePro.push_back(i + 1);
        firstIndexOriginal.push_back(idx[i] + 1);
        breakoutIndexinOrig.push_back(bo >= 0 ? bo + 1 : NA_INTEGER);
    }

    // assemble DataFrame
    DataFrame patternInfo = DataFrame::create(
        Named("PatternName")          = PatternName,
        Named("validPattern")         = validPattern,
        Named("firstIndexinPrePro")   = firstIndexPrePro,
        Named("firstIndexinOriginal") = firstIndexOriginal,
        Named("breakoutIndexinOrig")  = breakoutIndexinOrig
    );

    return List::create(
        Named("patternInfo") = patternInfo
    );
}
