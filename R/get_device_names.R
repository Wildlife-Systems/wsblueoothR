#' Get Unique Device Names from Bluetooth Data File or Result
#'
#' Extracts unique device names either from a Bluetooth data file or from 
#' the result of process_bluetooth(). When called after process_bluetooth(),
#' it uses the device names that were already collected during processing,
#' avoiding a second pass through the file.
#'
#' @param input Character string or bluetooth_data object. Either:
#'   - Path to input data file with format: device datetime address power name
#'   - Result object from process_bluetooth()
#' @param progress_interval Integer. Report progress every N lines (must be positive).
#'   Default is 10000. Only used when reading from a file.
#' @param verbose Logical. If TRUE (default), print processing messages to console.
#'   Set to FALSE to suppress output.
#'
#' @return A character vector of unique device names found in the data, sorted alphabetically.
#'   Empty names are included as empty strings.
#'
#' @examples
#' \dontrun{
#' # Method 1: Get names directly from file (requires reading entire file)
#' names <- get_device_names("data/combined_sort.txt")
#' 
#' # Method 2: Get names from process_bluetooth result (no extra file read)
#' data <- process_bluetooth("data/combined_sort.txt")
#' names <- get_device_names(data)
#' 
#' # See what names are present
#' head(names)
#' }
#'
#' @export
get_device_names <- function(input, progress_interval = 10000, verbose = TRUE) {
  
  # Check if input is a bluetooth_data object with device_names attribute
  if (inherits(input, "bluetooth_data") || inherits(input, "data.frame")) {
    device_names <- attr(input, "device_names")
    if (!is.null(device_names)) {
      if (isTRUE(verbose)) {
        message("Extracting device names from processed data...")
        message("Unique device names found: ", length(device_names))
      }
      return(device_names)
    }
  }
  
  # Otherwise, treat as file path
  input_file <- as.character(input)
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(paste("Error: Input file", input_file, "not found."))
  }
  
  # Validate progress_interval
  if (!is.null(progress_interval) && progress_interval <= 0) {
    stop("Error: progress_interval must be a positive integer.")
  }
  
  # Call Rcpp function
  if (isTRUE(verbose)) {
    message("Scanning for unique device names...")
  }
  result <- get_unique_device_names(input_file, as.integer(progress_interval))
  
  return(result)
}
