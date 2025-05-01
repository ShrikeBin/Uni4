#include "search.hpp"
#include "sorts.hpp"
#include <iostream>

int binsearch(std::vector<int>& arr, int target, Stats& stats) {
    int left = 0;
    int right = arr.size() - 1;

    while (left <= right) 
    {
        int mid = left + (right - left) / 2;

        if (arr[mid] == target) 
        {
            stats.comparisons++;
            return mid; // Target found
        } 
        else if (arr[mid] < target) 
        {
            stats.comparisons++;
            stats.comparisons++;
            left = mid + 1; // Search in the right half
        } 
        else 
        {
            stats.comparisons++;
            stats.comparisons++;
            right = mid - 1; // Search in the left half
        }
    }

    return -1; // Target not found
}