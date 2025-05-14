#!/usr/bin/env Rscript

# Compile the package with debugging enabled
# Ensure we're in the package directory
setwd("/Users/private/gits/ChartPatterns")

# Show current working directory
cat("Current working directory:", getwd(), "\n")

# Install any required dependencies
if (!require("Rcpp")) {
  install.packages("Rcpp")
}

# Generate Rcpp exports - this is critical for new functions
cat("Generating Rcpp exports for all Rcpp functions...\n")
tryCatch({
  Rcpp::compileAttributes()
  cat("Rcpp exports generated successfully.\n")
}, error = function(e) {
  cat("ERROR generating Rcpp exports:", e$message, "\n")
  quit(status = 1)
})

# Set compilation flags for debugging
Sys.setenv(PKG_CXXFLAGS="-g -O0 -std=c++17 -Wall")

# Check if UltraFastFind.cpp exists
if (file.exists("src/UltraFastFind.cpp")) {
  cat("Found UltraFastFind.cpp in src directory.\n")
} else {
  cat("WARNING: UltraFastFind.cpp not found in src directory!\n")
}

# Compile the package
cat("Compiling package with debugging enabled...\n")
compile_result <- system("R CMD INSTALL --preclean --with-keep.source .", intern = TRUE)
cat(paste(compile_result, collapse = "\n"), "\n")

cat("Compilation complete.\n")

# Validation check
cat("Checking if package can be loaded...\n")
tryCatch({
  library(ChartPatterns)
  cat("Package loaded successfully!\n")
  
  # List all exported functions
  cat("Functions available in the package:\n")
  print(ls("package:ChartPatterns"))
  
  # Check for UltraFastFind
  if ("UltraFastFind" %in% ls("package:ChartPatterns")) {
    cat("UltraFastFind function is available.\n")
  } else {
    cat("WARNING: UltraFastFind function not available in the package namespace.\n")
  }
}, error = function(e) {
  cat("Error loading package:", e$message, "\n")
})

# Done
cat("Script completed.\n") 