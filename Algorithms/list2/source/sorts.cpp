#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <functional>
#include <queue>

#include "sorts.hpp"

void std_sort(std::vector<int>& arr, SortStats& stats)
{
    std::sort(arr.begin(), arr.end());
}

void insertion_sort(std::vector<int>& arr, SortStats& stats)
{
    for (int i = 1; i < arr.size(); ++i)
    {
        int key = arr[i];
        int j = i - 1;
        
        while (j >= 0 && arr[j] > key)
        {
            arr[j + 1] = arr[j];
            --j;
            ++stats.comparisons;
            ++stats.swaps;
        }
        
        arr[j + 1] = key;
        ++stats.swaps;
    }
}

void quick_sort(std::vector<int>& arr, SortStats& stats)
{
    std::function<void(int, int)> quick_rec = [&](int left, int right)
    {
        if (left >= right) 
        {
            return;
        }
        
        int pivot = arr[(left + right) / 2];
        int i = left;
        int j = right;
        
        while (i <= j)
        {
            while (arr[i] < pivot)
            {
                ++i;
                ++stats.comparisons;
            }
            while (arr[j] > pivot)
            {
                --j;
                ++stats.comparisons;
            }
            
            if (i <= j)
            {
                std::swap(arr[i], arr[j]);
                ++stats.swaps;
                ++i;
                --j;
            }
        }
        
        quick_rec(left, j);
        quick_rec(i, right);
    };
    
    quick_rec(0, arr.size() - 1);
}

void dual_pivot_quick_sort(std::vector<int>& arr, SortStats& stats)
{
    std::function<void(int, int)> dual_pivot_quick_sort_impl = [&](int left, int right)
    {
        if (left >= right) 
        {
            return;
        }

        if (arr[left] > arr[right]) 
        {
            std::swap(arr[left], arr[right]);
            ++stats.comparisons;
            ++stats.swaps;
        }

        // two pivots, p and q
        int p = arr[left];
        int q = arr[right];
        int s = 0;
        int l= 0;
        int pPosition = left + s;
        int qPosition = right - l;

        std::cout << "P: " << p << " Q: " << q << std::endl;
        for(int i = left; i <= right; ++i)
        {
            std::cout << arr[i] << " ";
        }
        std::cout << std::endl;
        std::cout << pPosition << " " << qPosition << std::endl;

        // Count Strategy but incomplete
        for (int i = left + 1; i < right; ++i) 
        {
            std::cout << "i: " << i << std::endl;
            for(int j = left; j <= right; ++j)
            {
                std::cout << arr[j] << " ";
            }
            std::cout << std::endl;

            pPosition = left + s;
            qPosition = right - l;

            if(l > s)
            {
                if (arr[i] >= q) // co jeśli jest większe od q?? jak to przerzucic dobrze??
                {
                    ++stats.comparisons;
                    ++l;
                    std::swap(arr[i], arr[qPosition-1]);
                    ++stats.swaps;
                    std::swap(arr[qPosition-1], arr[qPosition]);
                    ++stats.swaps;
                } 
                else if (arr[i] <= p) 
                {
                    ++stats.comparisons;
                    ++s;
                    std::swap(arr[i], arr[pPosition]);
                    ++stats.swaps;
                }
                else    // is between p and q
                {
                    ++stats.comparisons;
                    ++stats.comparisons;
                }
            }
            else
            {
                if (arr[i] <= p) 
                {
                    ++stats.comparisons;
                    ++s;
                    std::swap(arr[i], arr[pPosition]);
                    ++stats.swaps;
                } 
                else if (arr[i] >= q) 
                {
                    ++stats.comparisons;
                    ++l;
                    std::swap(arr[i], arr[qPosition-1]);
                    ++stats.swaps;
                    std::swap(arr[qPosition-1], arr[qPosition]);
                    ++stats.swaps;
                }
                else    // is between p and q
                {
                    ++stats.comparisons;
                    ++stats.comparisons;
                }
            }
        }

        dual_pivot_quick_sort_impl(left, pPosition - 1);
        dual_pivot_quick_sort_impl(pPosition +1 , qPosition - 1);
        dual_pivot_quick_sort_impl(qPosition + 1, right);
    };
    
    dual_pivot_quick_sort_impl(0, arr.size() - 1);
}


void hybrid_sort(std::vector<int>& arr, SortStats& stats)
{
    std::function<void(int, int)> insort = [&](int left, int right)
    {
        for (int i = left; i <= right; ++i) 
        {
            int key = arr[i];
            int j = i - 1;
            
            while (j >= 0 && arr[j] > key) 
            {
                arr[j + 1] = arr[j];
                --j;
                ++stats.comparisons;
                ++stats.swaps;
            }
            
            arr[j + 1] = key;
            ++stats.swaps;
        }
    };

    std::function<void(int, int)> hybrid_quick = [&](int left, int right)
    {
        if (left >= right) 
        {
            return;
        }
        if (right - left < 10) 
        {
            insort(left, right);
            return;
        }
        
        int pivot = arr[(left + right) / 2];
        int i = left;
        int j = right;
        
        while (i <= j) 
        {
            while (arr[i] < pivot) 
            {
                ++i;
                ++stats.comparisons;
            }
            while (arr[j] > pivot) 
            {
                --j;
                ++stats.comparisons;
            }
            
            if (i <= j) 
            {
                std::swap(arr[i], arr[j]);
                ++stats.swaps;
                ++i;
                --j;
            }
        }
        
        hybrid_quick(left, j);
        hybrid_quick(i, right);
    };

    hybrid_quick(0, arr.size() - 1);
}

void merge_sort(std::vector<int>& arr, SortStats& stats)
{
    std::function<void(int, int)> merge_sort_rec = [&](int left, int right)
    {
        if (left >= right) 
        {
            return;
        }
        
        int mid = (left + right) / 2;
        merge_sort_rec(left, mid);
        merge_sort_rec(mid + 1, right);

        std::vector<int> tmp(right - left + 1);
        int i = left;
        int j = mid + 1;
        int k = 0;
        
        while (i <= mid && j <= right) 
        {
            if (arr[i] < arr[j]) 
            {
                tmp[k++] = arr[i++];
                ++stats.comparisons;
            } 
            else 
            {
                tmp[k++] = arr[j++];
                ++stats.comparisons;
            }
        }
    
        while (i <= mid) 
        {
            tmp[k++] = arr[i++];
            ++stats.swaps;
        }
        while (j <= right) 
        {
            tmp[k++] = arr[j++];
            ++stats.swaps;
        }
        
        for (int i = 0; i < k; ++i) 
        {
            arr[left + i] = tmp[i];
            ++stats.swaps;
        }
    };
    
    merge_sort_rec(0, arr.size() - 1);
}

void alt_merge_sort(std::vector<int>& arr, SortStats& stats)
{
    std::function<std::vector<std::pair<int, int>>()> find_rising_subsequences =
    [&]() 
    {
        std::vector<std::pair<int, int>> rising_subsequences;
        if (arr.empty()) return rising_subsequences;
        rising_subsequences.reserve(arr.size());
        int sequence_begin = 0;
        
        for (int i = 1; i < arr.size(); ++i) 
        {
            if (arr[i] < arr[i - 1]) 
            {
                rising_subsequences.emplace_back(sequence_begin, i - 1);
                sequence_begin = i;
            }
        }
        
        rising_subsequences.emplace_back(sequence_begin, arr.size() - 1);

        return rising_subsequences;
    };

    std::function<void(int, int, int, int)> merge = [&](int leftFirst, int leftLast, int rightFirst, int rightLast) 
    {
        if(leftFirst >= rightLast) 
        {
            return;
        }
        std::vector<int> tmp(rightLast - leftFirst + 1);
        int i = leftFirst;
        int j = rightFirst;
        int k = 0;

        while (i <= leftLast && j <= rightLast) 
        {
            if (arr[i] < arr[j]) 
            {
                tmp[k++] = arr[i++];
                ++stats.comparisons;
            } 
            else 
            {
                tmp[k++] = arr[j++];
                ++stats.comparisons;
            }
        }

        while (i <= leftLast) 
        {
            tmp[k++] = arr[i++];
            ++stats.swaps;
        }

        while (j <= rightLast) 
        {
            tmp[k++] = arr[j++];
            ++stats.swaps;
        }

        for (int i = 0; i < k; ++i) 
        {
            arr[leftFirst + i] = tmp[i];
            ++stats.swaps;
        }
    };

    std::queue<std::pair<int, int>> queue;
    std::vector<std::pair<int, int>> rising_subsequences = find_rising_subsequences();
    for(std::pair<int, int> sequence : rising_subsequences) 
    {
        queue.push(sequence);
    }

    std::pair<int, int> first;
    std::pair<int, int> second;

    while (queue.size() > 1) 
    {
        first = queue.front();
        queue.pop();
        second = queue.front();
        queue.pop();

        if(first.second > second.first) 
        {
            queue.push({first.first, first.second});
            first = queue.front();
            queue.pop();
            merge(second.first, second.second, first.first, first.second);
            queue.push({second.first, first.second});
        } 
        else 
        {
            merge(first.first, first.second, second.first, second.second);
            queue.push({first.first, second.second});
        }
    }
}
