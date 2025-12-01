test_that("get_address_duration works with synthetic data", {
  # Create temporary directory for test data
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_test.txt")
  
  # Generate synthetic data
  # Device 16 detects address AA:BB:CC:DD:EE:01 on 2025-08-15
  # First detection at 10:00:00, last at 10:30:00 (30 minutes = 1800 seconds)
  # 5 detections total
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-100500\tAA:BB:CC:DD:EE:01\t-48\tDevice1",
    "16\t20250815-101000\tAA:BB:CC:DD:EE:01\t-52\tDevice1",
    "16\t20250815-102000\tAA:BB:CC:DD:EE:01\t-49\tDevice1",
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-51\tDevice1",
    # Device 16 detects address AA:BB:CC:DD:EE:02 on same day
    # First at 14:00:00, last at 14:10:00 (10 minutes = 600 seconds)
    "16\t20250815-140000\tAA:BB:CC:DD:EE:02\t-55\tDevice2",
    "16\t20250815-141000\tAA:BB:CC:DD:EE:02\t-54\tDevice2",
    # Device 17 detects address AA:BB:CC:DD:EE:01 on 2025-08-15
    # First at 11:00:00, last at 12:00:00 (60 minutes = 3600 seconds)
    "17\t20250815-110000\tAA:BB:CC:DD:EE:01\t-60\tDevice1",
    "17\t20250815-113000\tAA:BB:CC:DD:EE:01\t-58\tDevice1",
    "17\t20250815-120000\tAA:BB:CC:DD:EE:01\t-59\tDevice1",
    # Device 16 detects address AA:BB:CC:DD:EE:01 on 2025-08-16 (different day)
    # First at 09:00:00, last at 09:05:00 (5 minutes = 300 seconds)
    "16\t20250816-090000\tAA:BB:CC:DD:EE:01\t-52\tDevice1",
    "16\t20250816-090500\tAA:BB:CC:DD:EE:01\t-50\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Test the function
  result <- get_address_duration(test_file, verbose = FALSE)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("device", "date", "address", "first_seen", "last_seen", 
                    "duration_seconds", "detection_count", "duration_minutes", 
                    "duration_hours") %in% names(result)))
  
  # Check number of records (4 unique device-date-address combinations)
  expect_equal(nrow(result), 4)
  
  # Check Device 16, 2025-08-15, AA:BB:CC:DD:EE:01
  row1 <- result[result$device == "16" & 
                 result$date == as.Date("2025-08-15") & 
                 result$address == "AA:BB:CC:DD:EE:01", ]
  expect_equal(nrow(row1), 1)
  expect_equal(row1$duration_seconds, 1800)  # 30 minutes
  expect_equal(row1$duration_minutes, 30)
  expect_equal(row1$duration_hours, 0.5)
  expect_equal(row1$detection_count, 5)
  expect_equal(row1$first_seen, "20250815-100000")
  expect_equal(row1$last_seen, "20250815-103000")
  
  # Check Device 16, 2025-08-15, AA:BB:CC:DD:EE:02
  row2 <- result[result$device == "16" & 
                 result$date == as.Date("2025-08-15") & 
                 result$address == "AA:BB:CC:DD:EE:02", ]
  expect_equal(nrow(row2), 1)
  expect_equal(row2$duration_seconds, 600)  # 10 minutes
  expect_equal(row2$detection_count, 2)
  
  # Check Device 17, 2025-08-15, AA:BB:CC:DD:EE:01
  row3 <- result[result$device == "17" & 
                 result$date == as.Date("2025-08-15") & 
                 result$address == "AA:BB:CC:DD:EE:01", ]
  expect_equal(nrow(row3), 1)
  expect_equal(row3$duration_seconds, 3600)  # 60 minutes
  expect_equal(row3$duration_hours, 1)
  expect_equal(row3$detection_count, 3)
  
  # Check Device 16, 2025-08-16, AA:BB:CC:DD:EE:01
  row4 <- result[result$device == "16" & 
                 result$date == as.Date("2025-08-16") & 
                 result$address == "AA:BB:CC:DD:EE:01", ]
  expect_equal(nrow(row4), 1)
  expect_equal(row4$duration_seconds, 300)  # 5 minutes
  expect_equal(row4$duration_minutes, 5)
  expect_equal(row4$detection_count, 2)
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles single detection correctly", {
  # Create temporary directory for test data
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_single_test.txt")
  
  # Single detection should have duration of 0
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  result <- get_address_duration(test_file, verbose = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$duration_seconds, 0)
  expect_equal(result$duration_minutes, 0)
  expect_equal(result$duration_hours, 0)
  expect_equal(result$detection_count, 1)
  expect_equal(result$first_seen, result$last_seen)
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles multiple files", {
  test_dir <- tempdir()
  test_file1 <- file.path(test_dir, "duration_multi1.txt")
  test_file2 <- file.path(test_dir, "duration_multi2.txt")
  
  # File 1: Device 16 on 2025-08-15
  synthetic_data1 <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-100500\tAA:BB:CC:DD:EE:01\t-48\tDevice1"
  )
  
  # File 2: Device 16 on same day, extending the duration
  synthetic_data2 <- c(
    "16\t20250815-101000\tAA:BB:CC:DD:EE:01\t-52\tDevice1",
    "16\t20250815-102000\tAA:BB:CC:DD:EE:01\t-49\tDevice1"
  )
  
  writeLines(synthetic_data1, test_file1)
  writeLines(synthetic_data2, test_file2)
  
  result <- get_address_duration(c(test_file1, test_file2), verbose = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$duration_seconds, 1200)  # 20 minutes total
  expect_equal(result$detection_count, 4)  # Combined count
  expect_equal(result$first_seen, "20250815-100000")
  expect_equal(result$last_seen, "20250815-102000")
  
  # Clean up
  unlink(c(test_file1, test_file2))
})

test_that("get_address_duration handles invalid datetime gracefully", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_invalid.txt")
  
  # Mix of valid and invalid datetimes
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\tinvalid-datetime\tAA:BB:CC:DD:EE:01\t-48\tDevice1",
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-51\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Should skip invalid lines and process valid ones
  result <- get_address_duration(test_file, verbose = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$duration_seconds, 1800)  # 30 minutes between valid detections
  expect_equal(result$detection_count, 2)  # Only valid detections counted
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration date conversion works correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_date_test.txt")
  
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250816-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20251201-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  result <- get_address_duration(test_file, verbose = FALSE)
  
  # Check that date column is Date class
  expect_s3_class(result$date, "Date")
  
  # Check specific dates
  expect_true(as.Date("2025-08-15") %in% result$date)
  expect_true(as.Date("2025-08-16") %in% result$date)
  expect_true(as.Date("2025-12-01") %in% result$date)
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration file validation works", {
  # Test with non-existent file
  expect_error(
    get_address_duration("nonexistent_file.txt", verbose = FALSE),
    "File\\(s\\) not found"
  )
  
  # Test with empty vector
  expect_error(
    get_address_duration(character(0), verbose = FALSE),
    "files must be a non-empty character vector"
  )
  
  # Test with non-character input
  expect_error(
    get_address_duration(123, verbose = FALSE),
    "files must be a non-empty character vector"
  )
})

test_that("get_address_duration sorting works correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_sort_test.txt")
  
  # Create data with different durations
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",  # 30 min duration
    "16\t20250815-100000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "16\t20250815-110000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",  # 60 min duration
    "17\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "17\t20250815-100500\tAA:BB:CC:DD:EE:01\t-50\tDevice1"   # 5 min duration
  )
  
  writeLines(synthetic_data, test_file)
  
  result <- get_address_duration(test_file, verbose = FALSE)
  
  # Should be sorted by device, date, then duration (descending)
  expect_equal(nrow(result), 3)
  
  # Device 16 records should come before device 17
  expect_equal(result$device[1], "16")
  expect_equal(result$device[2], "16")
  expect_equal(result$device[3], "17")
  
  # Within device 16, longer duration should come first
  expect_true(result$duration_seconds[1] > result$duration_seconds[2])
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles addresses with special characters", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_special_test.txt")
  
  # Test addresses with underscores and other characters
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice_1",
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-50\tDevice_1",
    "17\t20250815-100000\tAA_BB_CC_DD_EE_02\t-50\tDevice-2",
    "17\t20250815-101000\tAA_BB_CC_DD_EE_02\t-50\tDevice-2"
  )
  
  writeLines(synthetic_data, test_file)
  
  result <- get_address_duration(test_file, verbose = FALSE)
  
  # Should handle both addresses correctly
  expect_equal(nrow(result), 2)
  
  # Check first address
  row1 <- result[result$address == "AA:BB:CC:DD:EE:01", ]
  expect_equal(nrow(row1), 1)
  expect_equal(row1$duration_seconds, 1800)  # 30 minutes
  
  # Check second address with underscores
  row2 <- result[result$address == "AA_BB_CC_DD_EE_02", ]
  expect_equal(nrow(row2), 1)
  expect_equal(row2$duration_seconds, 600)  # 10 minutes
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles empty lines and malformed data", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_malformed_test.txt")
  
  # Mix of valid, empty, and malformed lines
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "",  # Empty line
    "16\t20250815\tAA:BB:CC:DD:EE:01",  # Missing fields
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "incomplete line",  # Completely malformed
    "16\t20250815-105000\tAA:BB:CC:DD:EE:01\t-50\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Should process only valid lines without crashing
  result <- get_address_duration(test_file, verbose = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$duration_seconds, 3000)  # 50 minutes between valid detections
  expect_equal(result$detection_count, 3)  # Only valid lines
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles large duration values", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_large_test.txt")
  
  # Detection spanning multiple hours
  synthetic_data <- c(
    "16\t20250815-080000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-200000\tAA:BB:CC:DD:EE:01\t-50\tDevice1"
  )
  
  writeLines(synthetic_data, test_file)
  
  result <- get_address_duration(test_file, verbose = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$duration_seconds, 43200)  # 12 hours
  expect_equal(result$duration_hours, 12)
  expect_equal(result$detection_count, 2)
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration handles large dataset without memory issues", {
  skip_on_cran()  # Skip on CRAN to avoid timeout
  
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_large_dataset.txt")
  
  # Generate synthetic data with many combinations to test memory handling
  # 10 devices x 30 days x 100 addresses = 30,000 unique combinations
  synthetic_data <- character()
  
  for (device in 16:25) {
    for (day in 1:30) {
      date_str <- sprintf("202508%02d", if (day <= 31) day else day - 31)
      for (addr_id in 1:100) {
        address <- sprintf("AA:BB:CC:DD:EE:%02X", addr_id)
        # Two detections per combination
        synthetic_data <- c(synthetic_data,
          sprintf("%d\t%s-100000\t%s\t-50\tDevice%d", device, date_str, address, device),
          sprintf("%d\t%s-110000\t%s\t-50\tDevice%d", device, date_str, address, device)
        )
      }
    }
  }
  
  writeLines(synthetic_data, test_file)
  
  # This should process without memory errors
  result <- get_address_duration(test_file, verbose = FALSE, progress_interval = 10000)
  
  # Verify we got results
  expect_true(nrow(result) > 0)
  expect_true(nrow(result) <= 30000)  # Should have at most 30k combinations
  
  # All durations should be 1 hour (3600 seconds)
  expect_true(all(result$duration_seconds == 3600))
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration device filter works correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_device_filter.txt")
  
  synthetic_data <- c(
    "16\t20250815-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-103000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "17\t20250815-100000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "17\t20250815-110000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "18\t20250815-100000\tAA:BB:CC:DD:EE:03\t-50\tDevice3",
    "18\t20250815-120000\tAA:BB:CC:DD:EE:03\t-50\tDevice3"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Filter for device 16 only
  result <- get_address_duration(test_file, verbose = FALSE, devices = "16")
  
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$device), "16")
  expect_equal(result$duration_seconds, 1800)
  
  # Filter for devices 16 and 17
  result2 <- get_address_duration(test_file, verbose = FALSE, devices = c("16", "17"))
  
  expect_equal(nrow(result2), 2)
  expect_true(all(result2$device %in% c("16", "17")))
  expect_false("18" %in% result2$device)
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration date filters work correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_date_filter.txt")
  
  synthetic_data <- c(
    "16\t20250801-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250801-110000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-100000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "16\t20250815-110000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "16\t20250831-100000\tAA:BB:CC:DD:EE:03\t-50\tDevice3",
    "16\t20250831-110000\tAA:BB:CC:DD:EE:03\t-50\tDevice3"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Filter for August 15 and later
  result1 <- get_address_duration(test_file, verbose = FALSE, 
                                  min_date = "2025-08-15")
  
  expect_equal(nrow(result1), 2)
  expect_true(all(result1$date >= as.Date("2025-08-15")))
  
  # Filter for August 15 and earlier
  result2 <- get_address_duration(test_file, verbose = FALSE, 
                                  max_date = "2025-08-15")
  
  expect_equal(nrow(result2), 2)
  expect_true(all(result2$date <= as.Date("2025-08-15")))
  
  # Filter for exact date range
  result3 <- get_address_duration(test_file, verbose = FALSE, 
                                  min_date = as.Date("2025-08-10"),
                                  max_date = as.Date("2025-08-20"))
  
  expect_equal(nrow(result3), 1)
  expect_equal(result3$date, as.Date("2025-08-15"))
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration combined filters work correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_combined_filter.txt")
  
  synthetic_data <- c(
    "16\t20250801-100000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250801-110000\tAA:BB:CC:DD:EE:01\t-50\tDevice1",
    "16\t20250815-100000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "16\t20250815-110000\tAA:BB:CC:DD:EE:02\t-50\tDevice2",
    "17\t20250815-100000\tAA:BB:CC:DD:EE:03\t-50\tDevice3",
    "17\t20250815-110000\tAA:BB:CC:DD:EE:03\t-50\tDevice3"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Filter for device 16 on August 15
  result <- get_address_duration(test_file, verbose = FALSE, 
                                 devices = "16",
                                 min_date = "2025-08-15",
                                 max_date = "2025-08-15")
  
  expect_equal(nrow(result), 1)
  expect_equal(result$device, "16")
  expect_equal(result$date, as.Date("2025-08-15"))
  
  # Clean up
  unlink(test_file)
})

test_that("get_address_duration name filters work correctly", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "duration_name_filter.txt")
  
  synthetic_data <- c(
    "1\t20250801-100000\tAA:BB:CC:DD:EE:01\t-50\tiPhone 13",
    "1\t20250801-110000\tAA:BB:CC:DD:EE:01\t-50\tiPhone 13",
    "1\t20250801-100000\tAA:BB:CC:DD:EE:02\t-50\tGalaxy S21",
    "1\t20250801-110000\tAA:BB:CC:DD:EE:02\t-50\tGalaxy S21",
    "1\t20250801-100000\tAA:BB:CC:DD:EE:03\t-50\tUnknown Device",
    "1\t20250801-110000\tAA:BB:CC:DD:EE:03\t-50\tUnknown Device",
    "1\t20250801-100000\tAA:BB:CC:DD:EE:04\t-50\t[TV] Samsung",
    "1\t20250801-110000\tAA:BB:CC:DD:EE:04\t-50\t[TV] Samsung"
  )
  
  writeLines(synthetic_data, test_file)
  
  # Test include_names filter
  result_include <- get_address_duration(test_file, verbose = FALSE, 
                                         include_names = c("iPhone", "Galaxy"))
  
  expect_equal(nrow(result_include), 2)
  expect_true(all(result_include$address %in% c("AA:BB:CC:DD:EE:01", "AA:BB:CC:DD:EE:02")))
  expect_equal(result_include$duration_seconds, c(3600, 3600))
  
  # Test exclude_names filter
  result_exclude <- get_address_duration(test_file, verbose = FALSE, 
                                         exclude_names = c("Unknown", "[TV]"))
  
  expect_equal(nrow(result_exclude), 2)
  expect_true(all(result_exclude$address %in% c("AA:BB:CC:DD:EE:01", "AA:BB:CC:DD:EE:02")))
  expect_equal(result_exclude$duration_seconds, c(3600, 3600))
  
  # Test combined include and exclude
  result_combined <- get_address_duration(test_file, verbose = FALSE,
                                          include_names = c("iPhone", "Galaxy", "Unknown"),
                                          exclude_names = "Unknown")
  
  expect_equal(nrow(result_combined), 2)
  expect_true(all(result_combined$address %in% c("AA:BB:CC:DD:EE:01", "AA:BB:CC:DD:EE:02")))
  
  # Clean up
  unlink(test_file)
})
