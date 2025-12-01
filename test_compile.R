#!/usr/bin/env Rscript
# Test script to compile and test duration analysis

cat("Loading package...\n")
devtools::load_all(".")

cat("\nRunning duration analysis tests...\n")
testthat::test_file("tests/testthat/test-duration_analysis.R")

cat("\nDone!\n")
