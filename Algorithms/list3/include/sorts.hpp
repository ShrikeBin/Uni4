#ifndef SORTS_HPP
#define SORTS_HPP

#include <string>
#include <functional>
#include <vector>
#include <cstddef>
#include <unordered_map>

struct SortStats 
{
    size_t comparisons = 0;
    size_t swaps = 0;

    void reset() 
    {
        comparisons = 0;
        swaps = 0;
    }
};

void quick_sort(std::vector<int>& arr, SortStats& stats);
void dual_pivot_quick_sort(std::vector<int>& arr, SortStats& stats);
void hybrid_sort(std::vector<int>& arr, SortStats& stats);

#endif