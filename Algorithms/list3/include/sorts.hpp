#ifndef SORTS_HPP
#define SORTS_HPP

#include <string>
#include <functional>
#include <vector>
#include <cstddef>
#include <unordered_map>

#include <stats.hpp>

void quick_sort(std::vector<int>& arr, Stats& stats);
void dual_pivot_quick_sort(std::vector<int>& arr, Stats& stats);
void hybrid_sort(std::vector<int>& arr, Stats& stats);
void better_dual_pivot_quick_sort(std::vector<int>& arr, Stats& stats);
void better_quick_sort(std::vector<int>& arr, Stats& stats);

#endif