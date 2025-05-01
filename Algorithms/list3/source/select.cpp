#include "select.hpp"
#include <algorithm>
#include <functional>
#include "sorts.hpp"
#include <iostream>


int RandomSelect(std::vector<int>& arr, int place, Stats& stats) 
{
    if (arr.empty() || place < 0 || place >= static_cast<int>(arr.size())) {
        std::cout<< "Invalid order value: " << place << "\n";
        std::cout<< "Array size: " << arr.size() << "\n";
        std::cout<< "you're tupid - PC's count from 0" << "\n";
        return -1; // Handle error appropriately
    }

    std::vector<int> working_arr = arr;

    std::function<int(int, int, int)> randomSelectImpl = [&](int start, int end, int k) -> int {
        while (true) 
        {
            if (start + 1 == end) 
            {
                return working_arr[start];
            }

            int pivot_idx = std::rand() % (end - start) + start;
            int pivot = working_arr[pivot_idx];

            // Three-way partition (Dutch National Flag) or smth
            int low = start;
            int mid = start;
            int high = end - 1;

            while (mid <= high) 
            {
                if (working_arr[mid] < pivot) 
                {
                    stats.comparisons++;
                    stats.swaps++;
                    std::swap(working_arr[low], working_arr[mid]);
                    ++low;
                    ++mid;
                } else if (working_arr[mid] == pivot) 
                {
                    stats.comparisons++;
                    stats.comparisons++; // bo wszedł najpierw do ifa
                    ++mid;
                } 
                else 
                {
                    stats.comparisons++;
                    stats.comparisons++;
                    stats.swaps++;
                    std::swap(working_arr[mid], working_arr[high]);
                    --high;
                }
            }

            int left_size = low - start;
            int equal_size = (high + 1) - low;

            if (k < left_size) 
            {
                end = low; // Recurse on left partition
            } 
            else if (k < left_size + equal_size) 
            {
                return pivot; // Found in equal partition
            } 
            else 
            {
                k -= (left_size + equal_size);
                start = high + 1; // Recurse on right partition
            }
        }
    };
    int result = randomSelectImpl(0, working_arr.size(), place);

    /*{
        std::vector<int> sorted_arr = arr;
        hybrid_sort(sorted_arr, stats);

        std::cout << "Array before selection: ";
        for(auto num : arr) std::cout << num << " ";
        std::cout << "\n";
        std::cout << place << "th order statistic is: " << result << "\n";

        if(sorted_arr[place] == result) std::cout << " - correct\n";
        else std::cout << " - incorrect\n";

        std::cout << "In sorted order: ";
        for(auto num : sorted_arr) std::cout << num << " ";
    }*/

    return result;
}

int Select(std::vector<int>& arr, int place, Stats& stats) {
    
    return ParametrizedSelect(arr, place, 5, stats);
}

int ParametrizedSelect(std::vector<int>& arr, int place, int parameter, Stats& stats) 
{
    if (arr.empty() || place < 0 || place >= static_cast<int>(arr.size()) || parameter < 1) {
        std::cout<< "Invalid order value: " << place << "\n";
        std::cout<< "Array size: " << arr.size() << "\n";
        std::cout<< "you're tupid - PC's count from 0" << "\n";
        return -1; // Handle error appropriately
    }

    auto getMedian = [&](std::vector<int> group) { // Copy intentional
        hybrid_sort(group, stats);
        return group[group.size()/2];
    };

    std::function<int(std::vector<int>, int)> select = [&](std::vector<int> nums, int k) -> int
    {
        if (nums.size() <= parameter) 
        {
            hybrid_sort(nums, stats);
            return nums[k];
        }

        // Create groups and collect medians
        std::vector<int> medians;
        for (size_t i = 0; i < nums.size(); i += parameter) 
        {
            auto start = nums.begin() + i;
            auto end = (i + parameter <= nums.size()) ? start + parameter : nums.end();
            std::vector<int> group(start, end);
            medians.push_back(getMedian(group));
        }

        // Recursively find median of medians
        int mom = select(medians, medians.size()/2);

        // Three-way partition
        std::vector<int> left, right;
        int equal_count = 0;
        for (int num : nums) 
        {
            if (num < mom) 
            {
                stats.comparisons++;
                left.push_back(num);
            }
            else if (num > mom) 
            {
                stats.comparisons++;
                right.push_back(num);
            }
            else 
            {
                equal_count++;
            }
        }

        // Determine partition to recurse on
        if (k < left.size()) return select(left, k);
        if (k < left.size() + equal_count) return mom;
        return select(right, k - left.size() - equal_count);
    };

    int result = select(arr, place);

    /*{
        std::vector<int> sorted_arr = arr;
        hybrid_sort(sorted_arr, stats);

        std::cout << "Array before selection: ";
        for(auto num : arr) std::cout << num << " ";
        std::cout << "\n";
        std::cout << place << "th order statistic is: " << result << "\n";

        if(sorted_arr[place] == result) std::cout << " - correct\n";
        else std::cout << " - incorrect\n";
        std::cout << "parameter was: " << parameter << "\n";

        std::cout << "In sorted order: ";
        for(auto num : sorted_arr) std::cout << num << " ";
    }*/

    return result;
}