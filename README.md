# wsbluetoothR

R package for processing Wildlife Systems Bluetooth data.

## Overview

`wsbluetoothR` provides fast C++-based processing of Bluetooth RSSI (Received Signal Strength Indicator) data files. The package aggregates device detections by datetime and supports flexible filtering based on device name prefixes.

## Features

- **High Performance**: C++ implementation using Rcpp for fast processing of large datasets
- **Multi-File Support**: Process multiple files simultaneously with automatic aggregation
- **Name Filtering**: Include or exclude records based on device name prefixes
- **Visualization Functions**: Built-in plotting for timeline analysis, prefix distribution, and detection heatmaps
- **Metadata Tracking**: Comprehensive processing statistics and device information

## Installation

Install from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("Wildlife-Systems/wsbluetoothR")
```

## Usage

### Basic Processing

```r
library(wsbluetoothR)

# Process a single Bluetooth data file
data <- process_bluetooth("data/bluetooth_data.txt")
```

## License

GPL-3

## Author

Ed Baker (ed@ebaker.me.uk)
