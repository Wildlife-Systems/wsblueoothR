#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <ctime>
#include <cstring>
#include <cstdlib>

using namespace Rcpp;

// Structure to store detection event
struct Detection {
    std::string device;
    std::string datetime;
    std::time_t timestamp;
    int power;
    
    Detection() : timestamp(0), power(-999) {}
    Detection(const std::string& dev, const std::string& dt, std::time_t ts, int pwr) 
        : device(dev), datetime(dt), timestamp(ts), power(pwr) {}
};

// Parse datetime string in format YYYYMMDD-HHMMSS to time_t
std::time_t parse_datetime_path(const std::string& datetime_str) {
    if (datetime_str.length() < 15) {
        return -1;
    }
    
    struct tm tm = {0};
    
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
    tm.tm_isdst = -1;
    
    return std::mktime(&tm);
}

//' Track Address Paths Through Devices
//'
//' For each address, creates a chronological path showing which devices detected
//' the address over time. Returns the top N addresses by detection count.
//'
//' @param input_files Character vector of file paths to process.
//' @param top_n Integer. Number of top addresses to return (by detection count). Default is 10.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param device_filter Character vector. Filter by specific device IDs. Empty = all devices.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{address}{Bluetooth MAC address}
//'     \item{detection_count}{Total number of detections}
//'     \item{device_count}{Number of unique devices that detected this address}
//'     \item{first_seen}{First detection timestamp (YYYYMMDD-HHMMSS)}
//'     \item{last_seen}{Last detection timestamp (YYYYMMDD-HHMMSS)}
//'     \item{path}{Chronological path through devices (e.g., "16 -> 18 -> 16 -> 21")}
//'   }
//'
//' @examples
//' \dontrun{
//' paths <- get_address_paths("data/combined_sort.txt", top_n = 20)
//' paths <- get_address_paths(c("file1.txt", "file2.txt"), top_n = 50)
//' }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_address_paths(std::vector<std::string> input_files,
                                  int top_n = 10,
                                  int progress_interval = 10000,
                                  Rcpp::CharacterVector device_filter = Rcpp::CharacterVector(),
                                  std::string min_date = "",
                                  std::string max_date = "") {
    
    // Convert device filter
    std::vector<std::string> device_filter_vec = Rcpp::as<std::vector<std::string>>(device_filter);
    std::unordered_set<std::string> device_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();
    
    // Map: address -> vector of detections
    std::unordered_map<std::string, std::vector<Detection>> address_detections;
    
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
            
            // Parse line: device datetime address power [name]
            std::istringstream iss(line);
            std::string device, datetime, address, power_str;
            
            if (!(iss >> device >> datetime >> address >> power_str)) {
                continue;
            }
            
            // Parse power as integer
            int power = std::atoi(power_str.c_str());
            
            // Apply device filter
            if (filter_device && device_set.find(device) == device_set.end()) {
                filtered_lines++;
                continue;
            }
            
            // Apply date filters
            std::string date_str = datetime.substr(0, 8);  // YYYYMMDD
            if (filter_min_date && date_str < min_date) {
                filtered_lines++;
                continue;
            }
            if (filter_max_date && date_str > max_date) {
                filtered_lines++;
                continue;
            }
            
            // Parse datetime
            std::time_t timestamp = parse_datetime_path(datetime);
            if (timestamp == -1) {
                continue;
            }
            
            // Store detection
            address_detections[address].push_back(Detection(device, datetime, timestamp, power));
        }
        
        file.close();
    }
    
    Rcout << "Total lines processed: " << total_lines << "\n";
    if (filtered_lines > 0) {
        Rcout << "Lines filtered out: " << filtered_lines << "\n";
    }
    Rcout << "Unique addresses found: " << address_detections.size() << "\n";
    
    // Sort detections for each address by timestamp and build paths
    struct AddressInfo {
        std::string address;
        int detection_count;
        int device_count;
        std::string first_seen;
        std::string last_seen;
        std::string path;
    };
    
    std::vector<AddressInfo> address_info;
    address_info.reserve(address_detections.size());
    
    size_t counter = 0;
    for (auto& pair : address_detections) {
        // Check for user interrupts periodically
        if (++counter % 1000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        const std::string& address = pair.first;
        std::vector<Detection>& detections = pair.second;
        
        // Sort by timestamp
        std::sort(detections.begin(), detections.end(),
                  [](const Detection& a, const Detection& b) {
                      return a.timestamp < b.timestamp;
                  });
        
        // Remove duplicates: when same timestamp, keep only highest power
        std::vector<Detection> filtered_detections;
        for (size_t i = 0; i < detections.size(); i++) {
            // Find all detections with same timestamp
            size_t j = i;
            while (j < detections.size() && detections[j].timestamp == detections[i].timestamp) {
                j++;
            }
            
            // If multiple detections at same time, find highest power
            if (j - i > 1) {
                size_t best_idx = i;
                for (size_t k = i + 1; k < j; k++) {
                    if (detections[k].power > detections[best_idx].power) {
                        best_idx = k;
                    }
                }
                filtered_detections.push_back(detections[best_idx]);
            } else {
                filtered_detections.push_back(detections[i]);
            }
            
            i = j - 1;  // Skip processed detections
        }
        
        // Build path (showing device transitions)
        // Remove cycles: if we see A -> B -> A within a short time window, simplify to A
        std::vector<std::pair<std::string, std::time_t>> path_nodes;  // device, timestamp
        std::unordered_set<std::string> unique_devices;
        
        for (const auto& det : filtered_detections) {
            unique_devices.insert(det.device);
            
            // Only add to path if device changed
            if (path_nodes.empty() || det.device != path_nodes.back().first) {
                path_nodes.push_back(std::make_pair(det.device, det.timestamp));
            }
        }
        
        // Remove short cycles (A -> B -> A where B appears for < 5 minutes)
        std::vector<std::string> simplified_path;
        simplified_path.reserve(path_nodes.size());
        
        for (size_t i = 0; i < path_nodes.size(); i++) {
            // Check if this creates a cycle with previous node
            if (i >= 2 && 
                path_nodes[i].first == path_nodes[i-2].first &&
                path_nodes[i].first != path_nodes[i-1].first) {
                
                // Check time difference: if middle node was brief (< 180 seconds), skip it
                std::time_t time_at_middle = path_nodes[i-1].second;
                std::time_t time_return = path_nodes[i].second;
                
                if (time_return - time_at_middle < 180) {  // 3 minutes
                    // Remove the last added node (the middle of the cycle)
                    if (!simplified_path.empty()) {
                        simplified_path.pop_back();
                    }
                    // Don't add current node (we stay at the original)
                    continue;
                }
            }
            
            simplified_path.push_back(path_nodes[i].first);
        }
        
        // Build path string
        std::string path;
        for (size_t i = 0; i < simplified_path.size(); i++) {
            if (i > 0) {
                path += " -> ";
            }
            path += simplified_path[i];
        }
        
        AddressInfo info;
        info.address = address;
        info.detection_count = filtered_detections.size();
        info.device_count = unique_devices.size();
        info.first_seen = filtered_detections.front().datetime;
        info.last_seen = filtered_detections.back().datetime;
        info.path = path;
        
        address_info.push_back(info);
    }
    
    // Sort by detection count (descending) and take top N
    std::sort(address_info.begin(), address_info.end(),
              [](const AddressInfo& a, const AddressInfo& b) {
                  return a.detection_count > b.detection_count;
              });
    
    // Take top N
    if (address_info.size() > static_cast<size_t>(top_n)) {
        address_info.resize(top_n);
    }
    
    Rcout << "Returning top " << address_info.size() << " addresses\n";
    
    // Convert to DataFrame
    std::vector<std::string> out_addresses, out_first_seen, out_last_seen, out_paths;
    std::vector<int> out_detection_counts, out_device_counts;
    
    for (const auto& info : address_info) {
        out_addresses.push_back(info.address);
        out_detection_counts.push_back(info.detection_count);
        out_device_counts.push_back(info.device_count);
        out_first_seen.push_back(info.first_seen);
        out_last_seen.push_back(info.last_seen);
        out_paths.push_back(info.path);
    }
    
    return DataFrame::create(
        Named("address") = out_addresses,
        Named("detection_count") = out_detection_counts,
        Named("device_count") = out_device_counts,
        Named("first_seen") = out_first_seen,
        Named("last_seen") = out_last_seen,
        Named("path") = out_paths,
        _["stringsAsFactors"] = false
    );
}
