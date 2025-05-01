#include "select.hpp"
#include <algorithm>

// na pewno da się szybciej to wszystko zrobić
int RandomSelect(std::vector<int>& arr, int place, SortStats& stats) 
{
    if (arr.empty() || place < 0 || place >= static_cast<int>(arr.size())) {
        return -1; // Handle error as needed
    }

    std::vector<int> working_arr = arr;

    std::function<int(int, int, int)> randomSelectImpl = [&](int start, int end, int k) -> int {
        while (true) {
            if (start + 1 == end) {
                return working_arr[start];
            }

            int pivot_idx = std::rand() % (end - start) + start;
            int pivot = working_arr[pivot_idx];

            // Three-way partition (Dutch National Flag)
            int low = start;
            int mid = start;
            int high = end - 1;

            while (mid <= high) {
                if (working_arr[mid] < pivot) {
                    std::swap(working_arr[low], working_arr[mid]);
                    ++low;
                    ++mid;
                } else if (working_arr[mid] == pivot) {
                    ++mid;
                } else {
                    std::swap(working_arr[mid], working_arr[high]);
                    --high;
                }
            }

            int left_size = low - start;
            int equal_size = (high + 1) - low;

            if (k < left_size) {
                end = low; // Recurse on left partition
            } else if (k < left_size + equal_size) {
                return pivot; // Found in equal partition
            } else {
                k -= (left_size + equal_size);
                start = high + 1; // Recurse on right partition
            }
        }
    };

    return randomSelectImpl(0, working_arr.size()-1, place);
}

int Select(std::vector<int>& arr, int place, SortStats& stats) {
    
    return ParametrizedSelect(arr, place, 5, stats);
}

int ParametrizedSelect(std::vector<int>& arr, int place, int parameter, SortStats& stats) 
{
    if (arr.empty() || place < 0 || place >= static_cast<int>(arr.size()) || parameter < 1) {
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
            if (num < mom) left.push_back(num);
            else if (num > mom) right.push_back(num);
            else equal_count++;
        }

        // Determine partition to recurse on
        if (k < left.size()) return select(left, k);
        if (k < left.size() + equal_count) return mom;
        return select(right, k - left.size() - equal_count);
    };

    return select(arr, place);
}