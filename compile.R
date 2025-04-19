#!/usr/bin/env Rscript

# Compile the package with debugging enabled
# Ensure we're in the package directory
setwd("/Users/private/gits/ChartPatterns")

# Show current working directory
cat("Current working directory:", getwd(), "\n")

# Set compilation flags for debugging
Sys.setenv(PKG_CXXFLAGS="-g -O0 -std=c++14 -Wall")

# Compile the package
cat("Compiling package with debugging enabled...\n")
system("R CMD INSTALL --preclean --with-keep.source .")

cat("Compilation complete.\n")

# Validation check
cat("Checking if package can be loaded...\n")
tryCatch({
  library(ChartPatterns)
  cat("Package loaded successfully!\n")
}, error = function(e) {
  cat("Error loading package:", e$message, "\n")
})

# Done
cat("Script completed.\n") 