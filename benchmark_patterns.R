# Benchmark script for ChartPatterns package functions
# Comparing all implementations including the new UltraFastFind

# Load required packages
library(ChartPatterns)
library(microbenchmark)
library(ggplot2)

# Create sample data for testing
set.seed(123)
create_test_data <- function(n = 1000, pattern_complexity = 0.1) {
  # Create time series with actual patterns for better benchmarking
  DATE <- as.Date("2020-01-01") + 0:(n-1)
  
  # Create a more realistic price series with some SHS patterns embedded
  PRICE <- cumsum(rnorm(n, 0, 1)) + 100
  
  # Add some actual patterns
  for (i in seq(1, n-100, 100)) {
    if (runif(1) > 0.5) {
      # Add SHS pattern
      center <- i + 50
      height <- PRICE[center] + runif(1, 5, 10)
      PRICE[center] <- height  # Head
      PRICE[center-20] <- height - runif(1, 2, 4)  # Left shoulder
      PRICE[center+20] <- height - runif(1, 2, 4)  # Right shoulder
      PRICE[center-30] <- height - runif(1, 6, 8)  # Left trough
      PRICE[center+30] <- height - runif(1, 6, 8)  # Right trough
    } else {
      # Add iSHS pattern
      center <- i + 50
      depth <- PRICE[center] - runif(1, 5, 10)
      PRICE[center] <- depth  # Head
      PRICE[center-20] <- depth + runif(1, 2, 4)  # Left shoulder
      PRICE[center+20] <- depth + runif(1, 2, 4)  # Right shoulder
      PRICE[center-30] <- depth + runif(1, 6, 8)  # Left peak
      PRICE[center+30] <- depth + runif(1, 6, 8)  # Right peak
    }
  }
  
  # Create pivot points (using a simple but realistic approach)
  # In real applications, you'd use a proper pivot detection algorithm
  
  # Find local minima and maxima
  is_min <- c(FALSE, diff(diff(PRICE) > 0) == 1, FALSE)
  is_max <- c(FALSE, diff(diff(PRICE) < 0) == 1, FALSE)
  indices <- which(is_min | is_max)
  
  # Ensure we have enough pivot points
  if (length(indices) < 10) {
    # Fallback to evenly spaced points if algorithm didn't find enough
    indices <- seq(1, n, by = 20)
  }
  
  list(DATE = DATE, PRICE = PRICE, indices = indices)
}

# Create test data with different sizes for scalability testing
small_data <- create_test_data(1000)
medium_data <- create_test_data(5000)
large_data <- create_test_data(10000)

# Function to run benchmark on a dataset
run_benchmark <- function(data, times = 10) {
  DATE <- data$DATE
  PRICE <- data$PRICE
  indices <- data$indices
  
  cat("Running benchmark with", length(DATE), "data points and", 
      length(indices), "pivot points\n")
  
  benchmark_results <- microbenchmark(
    findPatterns = {
      res1 <- do.call(cbind, ChartPatterns::findPatterns(indices-1, DATE, PRICE))
    },
    FastFindII = {
      res2 <- ChartPatterns::FastFindII(indices-1, DATE, PRICE)
    },
    FastFindII_chaosRegin = {
      res3 <- do.call(cbind, ChartPatterns::FastFindII_chaosRegin(indices-1, DATE, PRICE))
    },
    fastFind_chaosRegin = {
      res4 <- ChartPatterns::fastFind_chaosRegin(indices-1, DATE, PRICE)
    },
    UltraFastFind = {
      res5 <- ChartPatterns::UltraFastFind(indices-1, DATE, PRICE)
    },
    times = times
  )
  
  # Return the results
  return(benchmark_results)
}

# Run benchmarks with different dataset sizes
cat("==== Small Dataset Benchmark ====\n")
small_benchmark <- run_benchmark(small_data, times = 20)
print(small_benchmark)

cat("\n==== Medium Dataset Benchmark ====\n")
medium_benchmark <- run_benchmark(medium_data, times = 10)
print(medium_benchmark)

cat("\n==== Large Dataset Benchmark ====\n")
large_benchmark <- run_benchmark(large_data, times = 5)
print(large_benchmark)

# Plot the results
p1 <- autoplot(small_benchmark) + 
  ggtitle("Small Dataset (1,000 points)") +
  theme_minimal()

p2 <- autoplot(medium_benchmark) + 
  ggtitle("Medium Dataset (5,000 points)") +
  theme_minimal()

p3 <- autoplot(large_benchmark) + 
  ggtitle("Large Dataset (10,000 points)") +
  theme_minimal()

# Calculate relative performance
analyze_results <- function(benchmark_results) {
  summary_stats <- summary(benchmark_results)
  fastest <- min(summary_stats$median)
  summary_stats$relative <- summary_stats$median / fastest
  return(summary_stats[order(summary_stats$median), c("expr", "median", "relative")])
}

cat("\n==== Performance Analysis ====\n")
cat("Small Dataset Relative Performance:\n")
print(analyze_results(small_benchmark))

cat("\nMedium Dataset Relative Performance:\n")
print(analyze_results(medium_benchmark))

cat("\nLarge Dataset Relative Performance:\n")
print(analyze_results(large_benchmark))

# Save plots if needed
# ggsave("benchmark_small.png", p1, width = 10, height = 6)
# ggsave("benchmark_medium.png", p2, width = 10, height = 6)
# ggsave("benchmark_large.png", p3, width = 10, height = 6)

# Verify that all implementations produce the same results
validate_implementations <- function() {
  # Use a small fixed dataset for validation
  data <- create_test_data(2000)
  DATE <- data$DATE
  PRICE <- data$PRICE
  indices <- data$indices
  
  cat("\n==== Validation of Implementations ====\n")
  cat("Testing that all implementations find the same patterns...\n")
  
  res1 <- do.call(cbind, ChartPatterns::findPatterns(indices-1, DATE, PRICE))
  res2 <- ChartPatterns::FastFindII(indices-1, DATE, PRICE)
  res3 <- do.call(cbind, ChartPatterns::FastFindII_chaosRegin(indices-1, DATE, PRICE))
  res4 <- ChartPatterns::fastFind_chaosRegin(indices-1, DATE, PRICE)
  res5 <- ChartPatterns::UltraFastFind(indices-1, DATE, PRICE)
  
  # Compare number of patterns found
  cat("Number of patterns found:\n")
  cat("findPatterns:", if(is.null(res1)) 0 else nrow(res1$patternInfo), "\n")
  cat("FastFindII:", if(is.null(res2)) 0 else nrow(res2$patternInfo), "\n")
  cat("FastFindII_chaosRegin:", if(is.null(res3)) 0 else nrow(res3$patternInfo), "\n")
  cat("fastFind_chaosRegin:", if(is.null(res4)) 0 else nrow(res4$patternInfo), "\n")
  cat("UltraFastFind:", if(is.null(res5)) 0 else nrow(res5$patternInfo), "\n")
  
  # Compare pattern types found (if available)
  if(!is.null(res2) && !is.null(res5) && nrow(res2$patternInfo) > 0 && nrow(res5$patternInfo) > 0) {
    cat("\nPattern type distribution in FastFindII:\n")
    print(table(res2$patternInfo$PatternName, res2$patternInfo$validPattern))
    
    cat("\nPattern type distribution in UltraFastFind:\n")
    print(table(res5$patternInfo$PatternName, res5$patternInfo$validPattern))
  }
}

validate_implementations()

# Performance scaling analysis
analyze_scaling <- function() {
  s_stats <- analyze_results(small_benchmark)
  m_stats <- analyze_results(medium_benchmark)
  l_stats <- analyze_results(large_benchmark)
  
  scaling_data <- data.frame(
    Implementation = s_stats$expr,
    Small_Time_ms = s_stats$median / 1000,  # Convert to ms
    Medium_Time_ms = m_stats$median[match(s_stats$expr, m_stats$expr)] / 1000,
    Large_Time_ms = l_stats$median[match(s_stats$expr, l_stats$expr)] / 1000
  )
  
  scaling_data$Medium_Ratio = scaling_data$Medium_Time_ms / scaling_data$Small_Time_ms
  scaling_data$Large_Ratio = scaling_data$Large_Time_ms / scaling_data$Small_Time_ms
  
  cat("\n==== Performance Scaling Analysis ====\n")
  print(scaling_data)
  
  # Theoretical scaling would be linear = 5x for medium, 10x for large
  cat("\nIdeal scaling would be 5x for medium dataset and 10x for large dataset\n")
  cat("Values closer to this indicate better scaling efficiency\n")
}

analyze_scaling() 