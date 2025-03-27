#ifndef EXP_HPP
#define EXP_HPP

#include <vector>
#include <string>
#include <functional>
#include <unordered_map>
#include <chrono>
#include "sorts.hpp" 

void run_experiment(int n, int k, const std::string& mode, const std::unordered_map<std::string, std::function<void(std::vector<int>&, SortStats&)>>& sort_map);
void run_full_experiment(int k, const std::unordered_map<std::string, std::function<void(std::vector<int>&, SortStats&)>>& sort_map); 

std::vector<int> generate_random(int n);
std::vector<int> generate_ascending(int n);
std::vector<int> generate_descending(int n);

void write_stats_to_file(const std::string& filename, const SortStats& stats, int n, std::string mode, double time); 

#endif
