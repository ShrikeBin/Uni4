#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <functional>
#include <queue>

#include "sorts.hpp"
#include "select.hpp"

void better_quick_sort(std::vector<int>& arr, Stats& stats)
{
    std::function<void(int, int)> quick_rec = [&](int left, int right)
    {
        if (left >= right) 
        {
            return;
        }

        if(right >= arr.size() || left < 0)
        {
            std::cout << "Invalid range: left = " << left << ", right = " << right << ", arr.size() = " << arr.size() << std::endl;
            return;
        }

        std::vector<int> subarr(arr.begin() + left, arr.begin() + right + 1);
        int pivot = Select(subarr, (subarr.size() - 1) / 2, stats);
        stats.selects++;
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

void better_dual_pivot_quick_sort(std::vector<int>& arr, Stats& stats) 
{
    std::function<void(int, int)> dual_pivot_quick_sort_impl = [&](int left, int right) 
    {
        if (left >= right) return;
        
        if (arr[left] > arr[right]) 
        {
            std::swap(arr[left], arr[right]);
            stats.swaps++;
        }
        
        std::vector<int> subarr(arr.begin() + left, arr.begin() + right + 1);

        if(right >= arr.size() || left < 0)
        {
            std::cout << "Invalid range: left = " << left << ", right = " << right << ", arr.size() = " << arr.size() << std::endl;
            return;
        }

        int pivot1 = Select(subarr, (subarr.size() - 1) / 3, stats);
        int pivot2 = Select(subarr, ((subarr.size() - 1) * 2) / 3, stats);
        stats.selects += 2;
        int i = left + 1, lt = left + 1, gt = right - 1;
        
        while (i <= gt) 
        {
            ++stats.comparisons;
            if (arr[i] < pivot1) 
            {
                std::swap(arr[i], arr[lt]);
                stats.swaps++;
                lt++;
            } 
            else if (arr[i] > pivot2) 
            {
                stats.comparisons++;
                while (i < gt && arr[gt] > pivot2) 
                {
                    stats.comparisons++;
                    gt--;
                }

                std::swap(arr[i], arr[gt]);
                stats.swaps++;

                gt--;
                stats.comparisons++;

                if (arr[i] < pivot1) 
                {
                    std::swap(arr[i], arr[lt]);
                    stats.swaps++;
                    lt++;
                }
            }
            else
            {
                stats.comparisons++;
            }
            i++;
        }
        
        lt--;
        gt++;
        std::swap(arr[left], arr[lt]);
        std::swap(arr[right], arr[gt]);
        stats.swaps += 2;
        
        dual_pivot_quick_sort_impl(left, lt - 1);
        dual_pivot_quick_sort_impl(lt + 1, gt - 1);
        dual_pivot_quick_sort_impl(gt + 1, right);
    };
    
    dual_pivot_quick_sort_impl(0, arr.size() - 1);
}


void quick_sort(std::vector<int>& arr, Stats& stats) 
{
    std::function<void(int, int)> quick_rec = [&](int left, int right) 
    {
        if (left < right) 
        {
            int pivot = arr[right];
            int i = left - 1;

            for (int j = left; j < right; j++) 
            {
                stats.comparisons++;
                if (arr[j] <= pivot) 
                {
                    i++;
                    std::swap(arr[i], arr[j]);
                    stats.swaps++;
                }
            }

            std::swap(arr[i + 1], arr[right]);
            stats.swaps++;

            int p = i + 1;

            quick_rec(left, p - 1);
            quick_rec(p + 1, right);
        }
    };

    quick_rec(0, arr.size() - 1);
}


void dual_pivot_quick_sort(std::vector<int>& arr, Stats& stats) 
{
    std::function<void(int, int)> dual_pivot_quick_sort_impl = [&](int left, int right) 
    {
        if (left >= right) return;
        
        if (arr[left] > arr[right]) {
            std::swap(arr[left], arr[right]);
            stats.swaps++;
        }
        
        int pivot1 = arr[left], pivot2 = arr[right];
        int i = left + 1, lt = left + 1, gt = right - 1;
        
        while (i <= gt) 
        {
            stats.comparisons++;
            if (arr[i] < pivot1) 
            {
                std::swap(arr[i], arr[lt]);
                stats.swaps++;
                lt++;
            } 
            else if (arr[i] > pivot2) 
            {
                stats.comparisons++;
                while (i < gt && arr[gt] > pivot2) 
                {
                    stats.comparisons++;
                    gt--;
                }
                std::swap(arr[i], arr[gt]);
                stats.swaps++;
                gt--;
                stats.comparisons++;
                if (arr[i] < pivot1) 
                {
                    std::swap(arr[i], arr[lt]);
                    stats.swaps++;
                    lt++;
                }
            }
            else
            {
                stats.comparisons++;
            }
            i++;
        }
        
        lt--;
        gt++;
        std::swap(arr[left], arr[lt]);
        std::swap(arr[right], arr[gt]);
        stats.swaps += 2;
        
        dual_pivot_quick_sort_impl(left, lt - 1);
        dual_pivot_quick_sort_impl(lt + 1, gt - 1);
        dual_pivot_quick_sort_impl(gt + 1, right);
    };
    
    dual_pivot_quick_sort_impl(0, arr.size() - 1);
}


void hybrid_sort(std::vector<int>& arr, Stats& stats)
{
    std::function<void(int, int)> insort = [&](int left, int right)
    {
        for (int i = left; i <= right; ++i) 
        {
            int key = arr[i];
            int j = i - 1;
            
            ++stats.comparisons;
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
        if (right - left < 15) 
        {
            insort(left, right);
            return;
        }
        
        int pivot = arr[(left + right) / 2];
        int i = left;
        int j = right;
        
        while (i <= j) 
        {
            ++stats.comparisons;
            while (arr[i] < pivot) 
            {
                ++i;
                ++stats.comparisons;
            }

            ++stats.comparisons;
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