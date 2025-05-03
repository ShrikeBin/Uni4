#include "search.hpp"
#include "sorts.hpp"
#include <iostream>

int binsearch(std::vector<int>& arr, int target, Stats& stats) {
    int left = 0;
    int right = arr.size() - 1;

    while (left <= right) 
    {
        int mid = left + (right - left) / 2;

        stats.comparisons++;
        if (arr[mid] == target) 
        {
            std::cout << "Found " << target << " at index " << mid << std::endl;
            return mid; // Target found
        } 
        else if (arr[mid] < target) 
        {
            stats.comparisons++;
            left = mid + 1; // Search in the right half
        } 
        else 
        {
            stats.comparisons++;
            right = mid - 1; // Search in the left half
        }
    }

    return -1; // Target not found
}