#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace Rcpp;

// Helper function to check if name starts with any prefix in the list
bool starts_with_any(const std::string& name, const std::vector<std::string>& prefixes) {
  if (prefixes.empty()) {
    return false;
  }
  
  // Trim leading whitespace from name
  size_t start = 0;
  while (start < name.length() && std::isspace(name[start])) {
    start++;
  }
  
  std::string trimmed_name = name.substr(start);
  
  for (const auto& prefix : prefixes) {
    if (trimmed_name.length() >= prefix.length() &&
        trimmed_name.substr(0, prefix.length()) == prefix) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
DataFrame process_bluetooth_data(std::string input_file, 
                                 int progress_interval = 1000,
                                 Nullable<CharacterVector> include_prefixes = R_NilValue,
                                 Nullable<CharacterVector> exclude_prefixes = R_NilValue) {
  
  // Open input file
  std::ifstream file(input_file);
  if (!file.is_open()) {
    stop("Cannot open input file: " + input_file);
  }
  
  // Convert R prefix lists to C++ vectors
  std::vector<std::string> include_list;
  std::vector<std::string> exclude_list;
  
  if (include_prefixes.isNotNull()) {
    CharacterVector inc = as<CharacterVector>(include_prefixes);
    for (int i = 0; i < inc.size(); i++) {
      include_list.push_back(as<std::string>(inc[i]));
    }
  }
  
  if (exclude_prefixes.isNotNull()) {
    CharacterVector exc = as<CharacterVector>(exclude_prefixes);
    for (int i = 0; i < exc.size(); i++) {
      exclude_list.push_back(as<std::string>(exc[i]));
    }
  }
  
  // Hash map to store counts
  std::unordered_map<std::string, int> count_by_device;
  
  std::string line;
  int line_count = 0;
  
  // Process file line by line
  while (std::getline(file, line)) {
    line_count++;
    
    // Check for user interrupts periodically
    if (line_count % 10000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    
    // Output progress
    if (progress_interval > 0 && line_count % progress_interval == 0) {
      Rcout << "Processed " << line_count << " lines...\n";
      R_FlushConsole();
    }
    
    // Parse line: device datetime address power name
    std::istringstream iss(line);
    std::string device, datetime, address, power, name;
    
    if (iss >> device >> datetime >> address >> power) {
      // Get the rest of the line as name (optional)
      std::getline(iss, name);
      
      // Apply filtering based on name prefixes
      bool should_include = true;
      
      // If include list provided, name must start with one of the prefixes
      if (!include_list.empty()) {
        should_include = starts_with_any(name, include_list);
      }
      
      // If exclude list provided, name must NOT start with any of the prefixes
      if (should_include && !exclude_list.empty()) {
        should_include = !starts_with_any(name, exclude_list);
      }
      
      if (should_include) {
        // Create key from device and datetime
        std::string key = device + "_" + datetime;
        
        // Increment count
        count_by_device[key]++;
      }
    }
  }
  
  file.close();
  Rcout << "Total lines processed: " << line_count << "\n";
  
  // Convert map to vectors for DataFrame
  int total_count = count_by_device.size();
  std::vector<std::string> devices;
  std::vector<std::string> datetimes;
  std::vector<int> counts;
  
  devices.reserve(total_count);
  datetimes.reserve(total_count);
  counts.reserve(total_count);
  
  // Extract data from map
  for (const auto& pair : count_by_device) {
    // Split key into device and datetime
    std::string key = pair.first;
    size_t underscore_pos = key.find('_');
    
    if (underscore_pos != std::string::npos) {
      devices.push_back(key.substr(0, underscore_pos));
      datetimes.push_back(key.substr(underscore_pos + 1));
      counts.push_back(pair.second);
    }
  }
  
  // Sort by device then datetime
  std::vector<size_t> indices(devices.size());
  for (size_t i = 0; i < indices.size(); i++) {
    indices[i] = i;
  }
  
  std::sort(indices.begin(), indices.end(), [&](size_t a, size_t b) {
    if (devices[a] != devices[b]) {
      return devices[a] < devices[b];
    }
    return datetimes[a] < datetimes[b];
  });
  
  // Reorder vectors
  std::vector<std::string> sorted_devices(devices.size());
  std::vector<std::string> sorted_datetimes(devices.size());
  std::vector<int> sorted_counts(devices.size());
  
  for (size_t i = 0; i < indices.size(); i++) {
    sorted_devices[i] = devices[indices[i]];
    sorted_datetimes[i] = datetimes[indices[i]];
    sorted_counts[i] = counts[indices[i]];
  }
  
  // Create and return DataFrame
  return DataFrame::create(
    Named("device") = sorted_devices,
    Named("datetime") = sorted_datetimes,
    Named("count") = sorted_counts,
    _["stringsAsFactors"] = false
  );
}
