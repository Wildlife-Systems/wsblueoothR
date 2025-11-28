#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
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
CharacterVector get_unique_device_names(std::string input_file, 
                                        int progress_interval = 10000) {
  
  // Open input file
  std::ifstream file(input_file);
  if (!file.is_open()) {
    stop("Cannot open input file: " + input_file);
  }
  
  // Use unordered_set to store unique names
  std::unordered_set<std::string> unique_names;
  
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
      
      // Trim leading whitespace from name
      size_t start = 0;
      while (start < name.length() && std::isspace(name[start])) {
        start++;
      }
      
      // Trim trailing whitespace
      size_t end = name.length();
      while (end > start && std::isspace(name[end - 1])) {
        end--;
      }
      
      std::string trimmed_name = name.substr(start, end - start);
      
      // Add to set (empty names will be stored as empty string)
      unique_names.insert(trimmed_name);
    }
  }
  
  file.close();
  Rcout << "Total lines processed: " << line_count << "\n";
  Rcout << "Unique device names found: " << unique_names.size() << "\n";
  
  // Convert set to sorted vector
  std::vector<std::string> names_vector(unique_names.begin(), unique_names.end());
  
  // Sort alphabetically
  std::sort(names_vector.begin(), names_vector.end());
  
  // Convert to R CharacterVector
  CharacterVector result(names_vector.begin(), names_vector.end());
  
  return result;
}

// [[Rcpp::export]]
DataFrame find_common_prefixes_cpp(CharacterVector device_names, 
                                   int min_length = 3, 
                                   int min_count = 2,
                                   std::string stop_char = "") {
  
  // Convert to C++ vector and remove empty strings
  std::vector<std::string> names;
  for (int i = 0; i < device_names.size(); i++) {
    std::string name = as<std::string>(device_names[i]);
    if (name.length() > 0) {
      names.push_back(name);
    }
  }
  
  if (names.empty()) {
    return DataFrame::create(
      Named("prefix") = CharacterVector(0),
      Named("count") = IntegerVector(0),
      _["stringsAsFactors"] = false
    );
  }
  
  // Map from prefix to set of names with that prefix
  std::unordered_map<std::string, std::unordered_set<std::string>> prefix_map;
  
  // Extract all possible prefixes
  for (const auto& name : names) {
    int name_length = name.length();
    
    // If stop_char is specified, find its position
    int max_len = name_length;
    if (!stop_char.empty() && stop_char.length() > 0) {
      size_t stop_pos = name.find(stop_char[0]);
      if (stop_pos != std::string::npos) {
        max_len = stop_pos;
      }
    }
    
    if (max_len >= min_length) {
      // If stop_char is used, only add the longest prefix (up to stop_char)
      // Otherwise, get all prefixes from min_length to max_len
      if (!stop_char.empty() && stop_char.length() > 0) {
        std::string prefix = name.substr(0, max_len);
        prefix_map[prefix].insert(name);
      } else {
        for (int len = min_length; len <= max_len; len++) {
          std::string prefix = name.substr(0, len);
          prefix_map[prefix].insert(name);
        }
      }
    }
  }
  
  // Structure to hold results
  struct PrefixInfo {
    std::string prefix;
    int count;
    int prefix_length;
  };
  
  std::vector<PrefixInfo> results;
  
  // Filter by min_count and prepare results
  for (const auto& pair : prefix_map) {
    int count = pair.second.size();
    
    if (count >= min_count) {
      results.push_back({
        pair.first,
        count,
        static_cast<int>(pair.first.length())
      });
    }
  }
  
  // Sort by count (descending), then by prefix length (descending)
  std::sort(results.begin(), results.end(), 
    [](const PrefixInfo& a, const PrefixInfo& b) {
      if (a.count != b.count) {
        return a.count > b.count;
      }
      return a.prefix_length > b.prefix_length;
    });
  
  // Convert to R vectors
  CharacterVector prefix_vec(results.size());
  IntegerVector count_vec(results.size());
  
  for (size_t i = 0; i < results.size(); i++) {
    prefix_vec[i] = results[i].prefix;
    count_vec[i] = results[i].count;
  }
  
  return DataFrame::create(
    Named("prefix") = prefix_vec,
    Named("count") = count_vec,
    _["stringsAsFactors"] = false
  );
}

// [[Rcpp::export]]
DataFrame process_bluetooth_files(CharacterVector input_files,
                                  int progress_interval = 1000,
                                  Nullable<CharacterVector> include_prefixes = R_NilValue,
                                  Nullable<CharacterVector> exclude_prefixes = R_NilValue) {
  
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
  
  // Global hash map to store counts across all files
  // Reserve capacity to reduce rehashing (estimate ~1000 combinations per file)
  std::unordered_map<std::string, int> count_by_device;
  count_by_device.reserve(input_files.size() * 1000);
  
  // Set to store unique device names across all files
  std::unordered_set<std::string> unique_names;
  unique_names.reserve(500);  // Estimate typical number of unique device names
  
  int total_lines = 0;
  int total_filtered = 0;
  
  // Process each file
  for (int file_idx = 0; file_idx < input_files.size(); file_idx++) {
    std::string input_file = as<std::string>(input_files[file_idx]);
    
    Rcout << "\nProcessing file " << (file_idx + 1) << " of " << input_files.size() 
          << ": " << input_file << "\n";
    
    // Open input file
    std::ifstream file(input_file);
    if (!file.is_open()) {
      stop("Cannot open input file: " + input_file);
    }
    
    std::string line;
    int line_count = 0;
    int file_filtered = 0;
    
    // Process file line by line
    while (std::getline(file, line)) {
      line_count++;
      total_lines++;
      
      // Check for user interrupts periodically
      if (total_lines % 10000 == 0) {
        Rcpp::checkUserInterrupt();
      }
      
      // Output progress
      if (progress_interval > 0 && line_count % progress_interval == 0) {
        Rcout << "  Processed " << line_count << " lines...\n";
        R_FlushConsole();
      }
      
      // Parse line: device datetime address power name
      std::istringstream iss(line);
      std::string device, datetime, address, power, name;
      
      if (iss >> device >> datetime >> address >> power) {
        // Get the rest of the line as name (optional)
        std::getline(iss, name);
        
        // Apply filtering based on name prefixes FIRST
        bool should_include = true;
        
        // If include list provided, name must start with one of the prefixes
        if (!include_list.empty()) {
          should_include = starts_with_any(name, include_list);
        }
        
        // If exclude list provided, name must NOT start with any of the prefixes
        if (should_include && !exclude_list.empty()) {
          should_include = !starts_with_any(name, exclude_list);
        }
        
        // Only process records that pass the filter
        if (should_include) {
          // Trim leading whitespace from name for storage
          size_t start = 0;
          while (start < name.length() && std::isspace(name[start])) {
            start++;
          }
          
          // Trim trailing whitespace
          size_t end = name.length();
          while (end > start && std::isspace(name[end - 1])) {
            end--;
          }
          
          std::string trimmed_name = name.substr(start, end - start);
          
          // Store unique name (only for included records)
          unique_names.insert(trimmed_name);
          
          // Create key from device and datetime
          std::string key = device + "_" + datetime;
          
          // Increment count (aggregates across files automatically)
          count_by_device[key]++;
        } else {
          file_filtered++;
          total_filtered++;
        }
      }
    }
    
    file.close();
    Rcout << "  File lines processed: " << line_count << "\n";
    Rcout << "  File lines filtered: " << file_filtered << "\n";
  }
  
  Rcout << "\nTotal lines processed: " << total_lines << "\n";
  Rcout << "Total lines filtered out: " << total_filtered << "\n";
  Rcout << "Unique device names found: " << unique_names.size() << "\n";
  
  // Convert map to vectors for DataFrame
  size_t total_count = count_by_device.size();
  std::vector<std::string> devices;
  std::vector<std::string> datetimes;
  std::vector<int> counts;
  
  // Reserve exact capacity for efficiency
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
  
  // Convert unique names to sorted vector
  std::vector<std::string> names_vector(unique_names.begin(), unique_names.end());
  std::sort(names_vector.begin(), names_vector.end());
  
  // Create and return DataFrame
  DataFrame result = DataFrame::create(
    Named("device") = sorted_devices,
    Named("datetime") = sorted_datetimes,
    Named("count") = sorted_counts,
    _["stringsAsFactors"] = false
  );
  
  // Add metadata as attributes
  result.attr("total_lines") = total_lines;
  result.attr("lines_filtered") = total_filtered;
  result.attr("unique_combinations") = sorted_devices.size();
  result.attr("device_names") = CharacterVector(names_vector.begin(), names_vector.end());
  
  return result;
}
