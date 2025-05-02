#ifndef EXP_HPP
#define EXP_HPP

#include <vector>
#include <string>
#include <functional>
#include <unordered_map>
#include <chrono>
#include "sorts.hpp" 

void run_full(int k); 

std::vector<int> generate_random(int n);
std::vector<int> generate_ascending(int n);
std::vector<int> generate_descending(int n);

void write_stats_to_file(const std::string& filename, const Stats& stats, int n, std::string mode, double time); 

void run_select(int n, int repeats);
void run_rselect(int n, int repeats);
void run_pselect(int n, int repeats, int parameter);
void run_binsearch(int n, int repeats);
void run_quick(int n, int repeats);
void run_dpquick(int n, int repeats);

#endif
