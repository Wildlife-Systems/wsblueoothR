cat("Attempting to load package and capture errors...\n\n")

result <- tryCatch({
  devtools::load_all(".", recompile = TRUE, quiet = FALSE)
  cat("\n=== SUCCESS ===\n")
  TRUE
}, error = function(e) {
  cat("\n=== ERROR ===\n")
  cat("Error message:\n")
  print(e$message)
  cat("\n")
  FALSE
})

if (!result) {
  cat("\nTrying to get more details...\n")
  last_err <- try(rlang::last_error(), silent = TRUE)
  if (!inherits(last_err, "try-error")) {
    print(last_err)
  }
}
