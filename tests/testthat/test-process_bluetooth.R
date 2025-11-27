test_that("process_bluetooth works with test data", {
  # Get path to test data
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  
  # Skip if test data not found
  skip_if(test_file == "", "Test data file not found")
  
  # Process the test data
  result <- process_bluetooth(test_file, progress_interval = 10)
  
  # Check that result is a data.frame
  expect_s3_class(result, "data.frame")
  
  # Check that result has expected columns
  expect_true("device" %in% names(result))
  expect_true("datetime" %in% names(result))
  expect_true("count" %in% names(result))
  
  # Check that datetime is POSIXct
  expect_s3_class(result$datetime[1], "POSIXct")
  
  # Check that we have data
  expect_true(nrow(result) > 0)
  
  # Check that counts are positive integers
  expect_true(all(result$count > 0))
  expect_true(all(result$count == as.integer(result$count)))
})

test_that("process_bluetooth handles include_prefixes correctly", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Process with a prefix that likely won't match anything
  result_filtered <- process_bluetooth(test_file, 
                                       progress_interval = 10000,
                                       include_prefixes = c("XYZ_NoMatch"))
  
  # Should have no data rows (no names match)
  expect_equal(nrow(result_filtered), 0)
})

test_that("process_bluetooth handles exclude_prefixes correctly", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Process without filtering
  result_all <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Process with exclude (this will exclude all rows since names are empty/short)
  result_excluded <- process_bluetooth(test_file, 
                                      progress_interval = 10000,
                                      exclude_prefixes = c(""))
  
  # Excluded result should have fewer or equal rows
  expect_true(nrow(result_excluded) <= nrow(result_all))
})

test_that("process_bluetooth handles non-existent file", {
  expect_error(
    process_bluetooth("nonexistent_file_xyz123.txt"),
    "not found"
  )
})

test_that("process_bluetooth returns correct data types", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  result <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Check column types
  expect_type(result$device, "character")
  expect_s3_class(result$datetime, "POSIXct")
  expect_type(result$count, "integer")
})

test_that("datetime conversion handles correct format", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  result <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Check that there are no NA datetimes
  expect_true(all(!is.na(result$datetime)))
  
  # Check timezone is UTC
  expect_equal(attr(result$datetime, "tzone"), "UTC")
})
