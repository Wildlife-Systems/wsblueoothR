# Test duration analysis with small data file
library(wsbluetoothR)

# Test with 100 lines
cat("Testing with 100 lines...\n")
result <- get_address_duration("data/test_head100.txt", verbose = TRUE)
print(head(result))
cat("\nSuccess!\n")
