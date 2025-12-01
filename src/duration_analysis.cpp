#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <ctime>
#include <cstring>
#include <cstdlib>

using namespace Rcpp;

// Helper function to check if name starts with any prefix in the list
static bool starts_with_any(const std::string& name, const std::vector<std::string>& prefixes) {
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

// Structure to store time range and detection count
struct TimeRange {
    std::time_t first_seen;
    std::time_t last_seen;
    int count;
    std::string device;
    std::string date;
    std::string address;
    std::string first_seen_str;
    std::string last_seen_str;
    
    TimeRange() : first_seen(0), last_seen(0), count(0) {}
};

// Parse datetime string in format YYYYMMDD-HHMMSS to time_t
std::time_t parse_datetime(const std::string& datetime_str) {
    if (datetime_str.length() < 15) {
        return -1;
    }
    
    struct tm tm = {0};
    
    // Extract components: YYYYMMDD-HHMMSS
    char year[5], month[3], day[3], hour[3], min[3], sec[3];
    
    std::strncpy(year, datetime_str.c_str(), 4);
    year[4] = '\0';
    std::strncpy(month, datetime_str.c_str() + 4, 2);
    month[2] = '\0';
    std::strncpy(day, datetime_str.c_str() + 6, 2);
    day[2] = '\0';
    std::strncpy(hour, datetime_str.c_str() + 9, 2);
    hour[2] = '\0';
    std::strncpy(min, datetime_str.c_str() + 11, 2);
    min[2] = '\0';
    std::strncpy(sec, datetime_str.c_str() + 13, 2);
    sec[2] = '\0';
    
    tm.tm_year = std::atoi(year) - 1900;
    tm.tm_mon = std::atoi(month) - 1;
    tm.tm_mday = std::atoi(day);
    tm.tm_hour = std::atoi(hour);
    tm.tm_min = std::atoi(min);
    tm.tm_sec = std::atoi(sec);
    tm.tm_isdst = -1;  // Let mktime determine DST
    
    return std::mktime(&tm);
}

// Extract date string in format YYYY-MM-DD from datetime string
std::string extract_date(const std::string& datetime_str) {
    if (datetime_str.length() < 8) {
        return "";
    }
    
    std::string year = datetime_str.substr(0, 4);
    std::string month = datetime_str.substr(4, 2);
    std::string day = datetime_str.substr(6, 2);
    
    return year + "-" + month + "-" + day;
}

// Scan files to find all unique devices
std::vector<std::string> scan_unique_devices(const std::vector<std::string>& input_files,
                                             const std::vector<std::string>& device_filter_vec,
                                             const std::string& min_date,
                                             const std::string& max_date) {
    std::unordered_set<std::string> device_set_unique;
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();
    
    std::unordered_set<std::string> device_filter_set(device_filter_vec.begin(), device_filter_vec.end());
    
    for (const auto& input_file : input_files) {
        std::ifstream file(input_file);
        if (!file.is_open()) {
            continue;
        }
        
        std::string line;
        int line_count = 0;
        
        while (std::getline(file, line)) {
            line_count++;
            
            // Check for user interrupts periodically
            if (line_count % 50000 == 0) {
                Rcpp::checkUserInterrupt();
            }
            
            std::istringstream iss(line);
            std::string device, datetime;
            
            if (!(iss >> device >> datetime)) {
                continue;
            }
            
            // Apply device filter
            if (filter_device && device_filter_set.find(device) == device_filter_set.end()) {
                continue;
            }
            
            // Apply date filters
            std::string date_str = datetime.substr(0, 8);
            if (filter_min_date && date_str < min_date) {
                continue;
            }
            if (filter_max_date && date_str > max_date) {
                continue;
            }
            
            device_set_unique.insert(device);
        }
        
        file.close();
    }
    
    return std::vector<std::string>(device_set_unique.begin(), device_set_unique.end());
}

// Extract hour string from datetime in format YYYYMMDD-HHMMSS to YYYY-MM-DD-HH
std::string extract_datetime_hour(const std::string& datetime_str) {
    if (datetime_str.length() < 11) {
        return "";
    }
    
    std::string year = datetime_str.substr(0, 4);
    std::string month = datetime_str.substr(4, 2);
    std::string day = datetime_str.substr(6, 2);
    std::string hour = datetime_str.substr(9, 2);
    
    return year + "-" + month + "-" + day + "-" + hour;
}

//' Calculate Address Duration per Device and Day
//'
//' Calculates the time duration (in seconds) that each Bluetooth address
//' was detected by each device on each day. The duration is calculated as
//' the time difference between the first and last detection of that address
//' on that device on that day.
//'
//' @param input_files Character vector of file paths to process.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param low_memory Logical. If TRUE, processes one device at a time to reduce memory usage. Default is FALSE.
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{device}{Device ID}
//'     \item{date}{Date in YYYY-MM-DD format}
//'     \item{address}{Bluetooth MAC address}
//'     \item{first_seen}{Timestamp of first detection (YYYYMMDD-HHMMSS)}
//'     \item{last_seen}{Timestamp of last detection (YYYYMMDD-HHMMSS)}
//'     \item{duration_seconds}{Time duration in seconds between first and last detection}
//'     \item{detection_count}{Number of times this address was detected}
//'   }
//'
//' @examples
//' \dontrun{
//' durations <- calculate_address_duration("data/combined_sort.txt")
//' # Multiple files
//' durations <- calculate_address_duration(c("file1.txt", "file2.txt"))
//' }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_address_duration(std::vector<std::string> input_files,
                                     int progress_interval = 10000,
                                     Rcpp::CharacterVector device_filter = Rcpp::CharacterVector(),
                                     std::string min_date = "",
                                     std::string max_date = "",
                                     Rcpp::CharacterVector include_list = Rcpp::CharacterVector(),
                                     Rcpp::CharacterVector exclude_list = Rcpp::CharacterVector(),
                                     bool low_memory = false) {
    
    // Convert Rcpp vectors to std::vector
    std::vector<std::string> device_filter_vec = Rcpp::as<std::vector<std::string>>(device_filter);
    std::vector<std::string> include_list_vec = Rcpp::as<std::vector<std::string>>(include_list);
    std::vector<std::string> exclude_list_vec = Rcpp::as<std::vector<std::string>>(exclude_list);
    
    // Low-memory mode: process one device at a time
    if (low_memory) {
        Rcout << "Low-memory mode: scanning for unique devices...\n";
        R_FlushConsole();
        
        // First scan to find all unique devices
        std::vector<std::string> unique_devices = scan_unique_devices(input_files, device_filter_vec, min_date, max_date);
        
        Rcout << "Found " << unique_devices.size() << " unique device(s)\n";
        Rcout << "Processing devices one at a time...\n";
        R_FlushConsole();
        
        // Prepare result vectors
        std::vector<std::string> all_devices, all_dates, all_addresses, all_first_seen, all_last_seen;
        std::vector<double> all_durations;
        std::vector<int> all_counts;
        
        // Process each device separately
        for (size_t dev_idx = 0; dev_idx < unique_devices.size(); dev_idx++) {
            const std::string& current_device = unique_devices[dev_idx];
            
            Rcout << "Processing device " << (dev_idx + 1) << "/" << unique_devices.size() 
                  << " (ID: " << current_device << ")...\n";
            R_FlushConsole();
            
            // Create single-device filter
            Rcpp::CharacterVector single_device = Rcpp::CharacterVector::create(current_device);
            
            // Process this device (recursive call with low_memory=false)
            DataFrame device_result = calculate_address_duration(
                input_files, 0,  // progress_interval = 0 for inner calls
                single_device, min_date, max_date,
                include_list, exclude_list, false  // low_memory = false
            );
            
            // Append results
            CharacterVector dev_vec = device_result["device"];
            CharacterVector date_vec = device_result["date"];
            CharacterVector addr_vec = device_result["address"];
            CharacterVector first_vec = device_result["first_seen"];
            CharacterVector last_vec = device_result["last_seen"];
            NumericVector dur_vec = device_result["duration_seconds"];
            IntegerVector cnt_vec = device_result["detection_count"];
            
            for (int i = 0; i < dev_vec.size(); i++) {
                all_devices.push_back(as<std::string>(dev_vec[i]));
                all_dates.push_back(as<std::string>(date_vec[i]));
                all_addresses.push_back(as<std::string>(addr_vec[i]));
                all_first_seen.push_back(as<std::string>(first_vec[i]));
                all_last_seen.push_back(as<std::string>(last_vec[i]));
                all_durations.push_back(dur_vec[i]);
                all_counts.push_back(cnt_vec[i]);
            }
        }
        
        Rcout << "Low-memory processing complete. Total records: " << all_devices.size() << "\n";
        
        return DataFrame::create(
            Named("device") = all_devices,
            Named("date") = all_dates,
            Named("address") = all_addresses,
            Named("first_seen") = all_first_seen,
            Named("last_seen") = all_last_seen,
            Named("duration_seconds") = all_durations,
            Named("detection_count") = all_counts,
            _["stringsAsFactors"] = false
        );
    }
    
    // Normal mode: process all devices at once
    
    // Key: device_date_address -> TimeRange
    // Use unordered_map for O(1) lookups instead of O(log n)
    std::unordered_map<std::string, TimeRange> duration_map;
    duration_map.reserve(10000);  // Pre-allocate for typical dataset
    
    // Setup filters
    std::unordered_set<std::string> device_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();
    
    size_t total_lines = 0;
    size_t filtered_lines = 0;
    
    // Process each file
    for (const auto& input_file : input_files) {
        std::ifstream file(input_file);
        if (!file.is_open()) {
            Rcout << "Warning: Cannot open file: " << input_file << "\n";
            continue;
        }
        
        std::string line;
        int line_count = 0;
        
        while (std::getline(file, line)) {
            line_count++;
            total_lines++;
            
            // Check for user interrupts periodically
            if (line_count % 10000 == 0) {
                Rcpp::checkUserInterrupt();
            }
            
            // Output progress
            if (progress_interval > 0 && total_lines % progress_interval == 0) {
                Rcout << "Processed " << total_lines << " lines...\n";
                R_FlushConsole();
            }
            
            // Parse line: device datetime address power name
            std::istringstream iss(line);
            std::string device, datetime, address, power, name;
            
            if (!(iss >> device >> datetime >> address >> power)) {
                continue;  // Skip malformed lines
            }
            
            // Get the rest of the line as name (optional)
            std::getline(iss, name);
            
            // Apply name filtering (same logic as process_bluetooth)
            bool should_include = true;
            
            // If include list provided, name must start with one of the prefixes
            if (!include_list_vec.empty()) {
                should_include = starts_with_any(name, include_list_vec);
            }
            
            // If exclude list provided, name must NOT start with any of the prefixes
            if (should_include && !exclude_list_vec.empty()) {
                should_include = !starts_with_any(name, exclude_list_vec);
            }
            
            if (!should_include) {
                filtered_lines++;
                continue;
            }
            
            // Apply device filter
            if (filter_device && device_set.find(device) == device_set.end()) {
                filtered_lines++;
                continue;
            }
            
            // Extract date for filtering
            std::string date_str = datetime.substr(0, 8);  // YYYYMMDD
            
            // Apply date filters
            if (filter_min_date && date_str < min_date) {
                filtered_lines++;
                continue;
            }
            if (filter_max_date && date_str > max_date) {
                filtered_lines++;
                continue;
            }
            
            // Parse datetime to time_t
            std::time_t timestamp = parse_datetime(datetime);
            if (timestamp == -1) {
                continue;  // Skip invalid datetime
            }
            
            // Extract date
            std::string date = extract_date(datetime);
            if (date.empty()) {
                continue;
            }
            
            // Create key: device_date_address
            // Reserve space to avoid reallocations
            std::string key;
            key.reserve(device.length() + date.length() + address.length() + 2);
            key = device + "_" + date + "_" + address;
            
            // Update or create entry
            auto& entry = duration_map[key];
            
            if (entry.count == 0) {
                // First detection - store parsed components
                entry.first_seen = timestamp;
                entry.last_seen = timestamp;
                entry.count = 1;
                entry.device = device;
                entry.date = date;
                entry.address = address;
                
                // Format timestamps immediately
                char buf[20];
                std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", std::localtime(&timestamp));
                entry.first_seen_str = std::string(buf);
                entry.last_seen_str = entry.first_seen_str;
            } else {
                // Update time range
                if (timestamp < entry.first_seen) {
                    entry.first_seen = timestamp;
                    // Update first_seen string
                    char buf[20];
                    std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", std::localtime(&timestamp));
                    entry.first_seen_str = std::string(buf);
                }
                if (timestamp > entry.last_seen) {
                    entry.last_seen = timestamp;
                    // Update last_seen string
                    char buf[20];
                    std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", std::localtime(&timestamp));
                    entry.last_seen_str = std::string(buf);
                }
                entry.count++;
            }
        }
        
        file.close();
    }
    
    Rcout << "Total lines processed: " << total_lines << "\n";
    if (filtered_lines > 0) {
        Rcout << "Lines filtered out: " << filtered_lines << "\n";
    }
    Rcout << "Unique records: " << duration_map.size() << "\n";
    
    // Convert map to DataFrame
    std::vector<std::string> devices, dates, addresses, first_seen_strs, last_seen_strs;
    std::vector<double> duration_seconds;
    std::vector<int> detection_counts;
    
    devices.reserve(duration_map.size());
    dates.reserve(duration_map.size());
    addresses.reserve(duration_map.size());
    first_seen_strs.reserve(duration_map.size());
    last_seen_strs.reserve(duration_map.size());
    duration_seconds.reserve(duration_map.size());
    detection_counts.reserve(duration_map.size());
    
    size_t counter = 0;
    for (const auto& pair : duration_map) {
        // Check for user interrupts periodically
        if (++counter % 10000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        const TimeRange& tr = pair.second;
        
        // Calculate duration in seconds
        double duration = std::difftime(tr.last_seen, tr.first_seen);
        
        // Move pre-formatted data (no parsing or formatting needed!)
        devices.push_back(std::move(tr.device));
        dates.push_back(std::move(tr.date));
        addresses.push_back(std::move(tr.address));
        first_seen_strs.push_back(std::move(tr.first_seen_str));
        last_seen_strs.push_back(std::move(tr.last_seen_str));
        duration_seconds.push_back(duration);
        detection_counts.push_back(tr.count);
    }
    
    // Clear map to free memory before creating DataFrame
    duration_map.clear();
    
    return DataFrame::create(
        Named("device") = devices,
        Named("date") = dates,
        Named("address") = addresses,
        Named("first_seen") = first_seen_strs,
        Named("last_seen") = last_seen_strs,
        Named("duration_seconds") = duration_seconds,
        Named("detection_count") = detection_counts,
        _["stringsAsFactors"] = false
    );
}

//' Calculate Average Address Duration per Device and Time Period
//'
//' Calculates statistics (median, mean, etc.) for address durations across all Bluetooth addresses
//' for each device grouped by time period (hour or day). This gives you the typical
//' detection duration for a device in each time window.
//'
//' @param input_files Character vector of file paths to process.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param device_filter Character vector. Filter by specific device IDs. Empty = all devices.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//' @param include_list Character vector. Only include records where name starts with these prefixes.
//' @param exclude_list Character vector. Exclude records where name starts with these prefixes.
//' @param time_group String. Time grouping: "day" or "hour". Default is "day".
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{device}{Device ID}
//'     \item{datetime}{Time period (YYYY-MM-DD for day, YYYY-MM-DD-HH for hour)}
//'     \item{median_duration_seconds}{Median duration across all addresses in this period}
//'     \item{mean_duration_seconds}{Mean duration across all addresses in this period}
//'     \item{min_duration_seconds}{Minimum duration}
//'     \item{max_duration_seconds}{Maximum duration}
//'     \item{address_count}{Number of unique addresses detected}
//'     \item{total_detections}{Total detection count}
//'   }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_average_address_duration(std::vector<std::string> input_files,
                                           int progress_interval = 10000,
                                           Rcpp::CharacterVector device_filter = Rcpp::CharacterVector(),
                                           std::string min_date = "",
                                           std::string max_date = "",
                                           Rcpp::CharacterVector include_list = Rcpp::CharacterVector(),
                                           Rcpp::CharacterVector exclude_list = Rcpp::CharacterVector(),
                                           bool low_memory = false,
                                           std::string time_group = "day") {
    
    // First get daily durations using existing function
    DataFrame daily = calculate_address_duration(input_files, progress_interval,
                                                 device_filter, min_date, max_date,
                                                 include_list, exclude_list, low_memory);
    
    // Extract columns
    std::vector<std::string> devices = as<std::vector<std::string>>(daily["device"]);
    std::vector<std::string> dates = as<std::vector<std::string>>(daily["date"]);
    std::vector<std::string> addresses = as<std::vector<std::string>>(daily["address"]);
    std::vector<std::string> first_seen_strs = as<std::vector<std::string>>(daily["first_seen"]);
    std::vector<double> durations = as<std::vector<double>>(daily["duration_seconds"]);
    std::vector<int> counts = as<std::vector<int>>(daily["detection_count"]);
    
    // Build map: device_datetime -> vector of durations
    std::unordered_map<std::string, std::vector<double>> duration_map;
    std::unordered_map<std::string, std::unordered_set<std::string>> address_map;
    std::unordered_map<std::string, int> detection_map;
    
    for (size_t i = 0; i < devices.size(); i++) {
        // Check for user interrupts periodically
        if (i % 10000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        // Build key based on time_group
        std::string time_key;
        if (time_group == "hour") {
            time_key = extract_datetime_hour(first_seen_strs[i]);
        } else {
            time_key = dates[i];
        }
        
        std::string key = devices[i] + "_" + time_key;
        duration_map[key].push_back(durations[i]);
        address_map[key].insert(addresses[i]);
        detection_map[key] += counts[i];
    }
    
    // Calculate statistics for each device-time period pair
    std::vector<std::string> out_devices, out_datetimes;
    std::vector<double> out_median, out_mean, out_min, out_max;
    std::vector<int> out_address_count, out_detections;
    
    size_t counter = 0;
    for (const auto& pair : duration_map) {
        // Check for user interrupts periodically
        if (++counter % 1000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        const std::string& key = pair.first;
        const std::vector<double>& vals = pair.second;
        
        // Parse key: device_datetime
        size_t underscore = key.find('_');
        std::string device = key.substr(0, underscore);
        std::string datetime = key.substr(underscore + 1);
        
        // Calculate median
        std::vector<double> sorted_vals = vals;
        std::sort(sorted_vals.begin(), sorted_vals.end());
        double median;
        size_t n = sorted_vals.size();
        if (n % 2 == 0) {
            median = (sorted_vals[n/2 - 1] + sorted_vals[n/2]) / 2.0;
        } else {
            median = sorted_vals[n/2];
        }
        
        // Calculate mean
        double sum = 0;
        for (double v : vals) sum += v;
        double mean = sum / vals.size();
        
        // Min and max
        double min_val = *std::min_element(vals.begin(), vals.end());
        double max_val = *std::max_element(vals.begin(), vals.end());
        
        // Store results
        out_devices.push_back(device);
        out_datetimes.push_back(datetime);
        out_median.push_back(median);
        out_mean.push_back(mean);
        out_min.push_back(min_val);
        out_max.push_back(max_val);
        out_address_count.push_back(address_map[key].size());
        out_detections.push_back(detection_map[key]);
    }
    
    // Create DataFrame
    DataFrame result = DataFrame::create(
        Named("device") = out_devices,
        Named("datetime") = out_datetimes,
        Named("median_duration_seconds") = out_median,
        Named("mean_duration_seconds") = out_mean,
        Named("min_duration_seconds") = out_min,
        Named("max_duration_seconds") = out_max,
        Named("address_count") = out_address_count,
        Named("total_detections") = out_detections,
        _["stringsAsFactors"] = false
    );
    
    return result;
}
