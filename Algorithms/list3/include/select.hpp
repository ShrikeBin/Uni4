#ifndef SELECT_HPP
#define SELECT_HPP

#include <vector>
#include "sorts.hpp"

int Select(std::vector<int>& arr, int place, SortStats& stats);
int RandomSelect(std::vector<int>& arr, int place, SortStats& stats);
int ParametrizedSelect(std::vector<int>& arr, int place, int parameter, SortStats& stats);

#endif