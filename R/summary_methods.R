#' Summary Method for Bluetooth Data
#'
#' Provides a summary of Bluetooth data processing results.
#'
#' @param object A bluetooth_data object from \code{process_bluetooth()}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the object. Prints summary to console.
#' @importFrom stats aggregate median
#' @export
summary.bluetooth_data <- function(object, ...) {
  message("\n=== Bluetooth Data Summary ===\n")
  
  # File information
  if (!is.null(attr(object, "file_name"))) {
  message("File: ", attr(object, "file_name"))
  }
  if (!is.null(attr(object, "file_size"))) {
    file_size_mb <- round(attr(object, "file_size") / 1024^2, 2)
  message("File size: ", file_size_mb, " MB")
  }
  
  # Processing statistics
  if (!is.null(attr(object, "total_lines"))) {
  message("\nProcessing Statistics:")
  message("  Total lines processed: ", attr(object, "total_lines"))
  }
  if (!is.null(attr(object, "lines_filtered"))) {
    lines_filtered <- attr(object, "lines_filtered")
    total_lines <- attr(object, "total_lines")
    if (!is.null(total_lines) && total_lines > 0) {
      pct_filtered <- round(100 * lines_filtered / total_lines, 2)
      message("  Lines filtered out: ", lines_filtered, 
              paste0("(", pct_filtered, "%)"))
    } else {
      message("  Lines filtered out: ", lines_filtered)
    }
  }
  if (!is.null(attr(object, "unique_combinations"))) {
  message("  Unique device-datetime combinations: ", attr(object, "unique_combinations"))
  }
  if (!is.null(attr(object, "device_names"))) {
  message("  Unique device names: ", length(attr(object, "device_names")))
  }
  if (!is.null(attr(object, "processing_time"))) {
  message("  Processing time: ", round(attr(object, "processing_time"), 2), " seconds")
  }
  
  # Data summary
  message("\nData Summary:")
  message("  Number of rows: ", nrow(object))
  
  # Device summary
  devices <- unique(object$device)
  message("  Unique devices: ", length(devices))
  if (length(devices) <= 10) {
    message("    Devices: ", paste(devices, collapse = ", "))
  }
  
  # Time range
  if ("datetime" %in% names(object) && nrow(object) > 0) {
    time_range <- range(object$datetime, na.rm = TRUE)
    message("  Time range: ", format(time_range[1]), " to ", format(time_range[2]))
    duration <- difftime(time_range[2], time_range[1], units = "days")
    message("  Duration: ", round(as.numeric(duration), 2), " days")
  }
  
  # Detection summary
  if ("count" %in% names(object)) {
    message("\nDetection Counts:")
    message("  Total detections: ", sum(object$count))
    message("  Mean detections per combination: ", round(mean(object$count), 2))
    message("  Median detections: ", median(object$count))
    message("  Range: ", min(object$count), " - ", max(object$count))
  }
  
  message("")
  invisible(object)
}

#' Print Method for Bluetooth Data
#'
#' @param x A bluetooth_data object.
#' @param ... Additional arguments passed to print.data.frame.
#'
#' @export
print.bluetooth_data <- function(x, ...) {
  message("Bluetooth Data: ", nrow(x), " rows")
  if (!is.null(attr(x, "file_name"))) {
    message("Source: ", attr(x, "file_name"))
  }
  message("")
  NextMethod("print")
}
