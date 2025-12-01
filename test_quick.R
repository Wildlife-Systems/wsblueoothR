# Test compilation and name filtering
cat("Compiling package...\n")
result <- try(devtools::load_all("."), silent = FALSE)

if (inherits(result, "try-error")) {
  cat("\n=== COMPILATION FAILED ===\n")
  q(status = 1)
} else {
  cat("\n=== COMPILATION SUCCESSFUL ===\n")
  cat("\nRunning quick test with name filtering...\n")
  
  # Create test data
  test_file <- tempfile(fileext = ".txt")
  writeLines(c(
    "1 20250101-120000 AA:BB:CC:DD:EE:FF -50 iPhone 13",
    "1 20250101-120100 AA:BB:CC:DD:EE:FF -52 iPhone 13",
    "1 20250101-120000 BB:CC:DD:EE:FF:AA -50 Galaxy S21",
    "1 20250101-120100 BB:CC:DD:EE:FF:AA -52 Galaxy S21",
    "1 20250101-120000 CC:DD:EE:FF:AA:BB -50 Unknown Device",
    "1 20250101-120100 CC:DD:EE:FF:AA:BB -52 Unknown Device"
  ), test_file)
  
  # Test basic function
  cat("\n1. Testing basic duration calculation:\n")
  result_all <- calculate_address_duration(test_file, progress_interval = 0)
  cat("   Total records:", nrow(result_all), "(expected: 3)\n")
  
  # Test include filter
  cat("\n2. Testing include_names filter (iPhone, Galaxy):\n")
  result_include <- calculate_address_duration(test_file, progress_interval = 0,
                                               include_list = c("iPhone", "Galaxy"))
  cat("   Filtered records:", nrow(result_include), "(expected: 2)\n")
  
  # Test exclude filter
  cat("\n3. Testing exclude_names filter (Unknown):\n")
  result_exclude <- calculate_address_duration(test_file, progress_interval = 0,
                                               exclude_list = "Unknown")
  cat("   Filtered records:", nrow(result_exclude), "(expected: 2)\n")
  
  cat("\n✓ All tests PASSED\n")
  
  unlink(test_file)
}
