# Just try to compile
cat("Attempting compilation...\n")
result <- try({
  devtools::load_all(".", recompile = TRUE, quiet = FALSE)
}, silent = FALSE)

if (inherits(result, "try-error")) {
  cat("\n=== COMPILATION FAILED ===\n")
  cat("See errors above\n")
} else {
  cat("\n=== COMPILATION SUCCESSFUL ===\n")
}
