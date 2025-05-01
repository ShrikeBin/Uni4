#ifndef SELECT_HPP
#define SELECT_HPP

#include <vector>
#include "stats.hpp"

int Select(std::vector<int>& arr, int place, Stats& stats);
int RandomSelect(std::vector<int>& arr, int place, Stats& stats);
int ParametrizedSelect(std::vector<int>& arr, int place, int parameter, Stats& stats);

#endif