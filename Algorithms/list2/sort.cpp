#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <functional>

void std_sort(std::vector<int>& arr)
{
    std::sort(arr.begin(), arr.end());
}

void insertion_sort(std::vector<int>& arr)
{
    for (int i = 1; i < arr.size(); ++i) 
    {
        int key = arr[i];
        int j = i - 1;
        
        while (j >= 0 && arr[j] > key) 
        {
            arr[j + 1] = arr[j];
            --j;
        }
        
        arr[j + 1] = key;
    }
}

void quick_sort(std::vector<int>& arr)
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
            }
            while (arr[j] > pivot) 
            {
                --j;
            }
            
            if (i <= j) 
            {
                std::swap(arr[i], arr[j]);
                ++i;
                --j;
            }
        }
        
        quick_rec(left, j);
        quick_rec(i, right);
    };
    
    quick_rec(0, arr.size() - 1);
}

void dual_pivot_quick_sort(std::vector<int>& arr)
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
        }
        
        int pivot1 = arr[left];
        int pivot2 = arr[right];
        int i = left + 1;
        int k = left + 1;
        int j = right - 1;
        
        while (k <= j) 
        {
            if (arr[k] < pivot1) 
            {
                std::swap(arr[k], arr[i]);
                ++i;
            } 
            else if (arr[k] >= pivot2) 
            {
                while (arr[j] > pivot2 && k < j) 
                {
                    --j;
                }
                std::swap(arr[k], arr[j]);
                --j;
                
                if (arr[k] < pivot1) 
                {
                    std::swap(arr[k], arr[i]);
                    ++i;
                }
            }
            ++k;
        }
        
        --i;
        ++j;
        std::swap(arr[left], arr[i]);
        std::swap(arr[right], arr[j]);
        
        dual_pivot_quick_sort_impl(left, i - 1);
        dual_pivot_quick_sort_impl(i + 1, j - 1);
        dual_pivot_quick_sort_impl(j + 1, right);
    };
    
    dual_pivot_quick_sort_impl(0, arr.size() - 1);
}

void hybrid_sort(std::vector<int>& arr)
{
    if (arr.size() < 10) 
    {
        insertion_sort(arr);
    } 
    else 
    {
        quick_sort(arr);
    }
}

void merge_sort(std::vector<int>& arr)
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

        // merge is down there
        std::vector<int> tmp(right - left + 1);
        int i = left;
        int j = mid + 1;
        int k = 0;
        
        while (i <= mid && j <= right) 
        {
            if (arr[i] < arr[j]) 
            {
                tmp[k++] = arr[i++];
            } 
            else 
            {
                tmp[k++] = arr[j++];
            }
        }
    
        while (i <= mid) 
        {
            tmp[k++] = arr[i++];
        }
        while (j <= right) 
        {
            tmp[k++] = arr[j++];
        }
        
        for (int i = 0; i < k; ++i) 
        {
            arr[left + i] = tmp[i];
        }
    };
    
    merge_sort_rec(0, arr.size() - 1);
}

void alt_merge_sort(std::vector<int>& arr)
{
    // should use Merge from mergesort and D&C and rising subsequences 
    // TimSort or PowerSort
    // finds biggest rising subsequence from 'left' and then merges it with the rest
    // I DONT FREAKING KNOW

    std::function<std::vector<std::vector<int>>(std::vector<int>&)> find_rising_subsequences = [&](std::vector<int>& arr)
    {
        std::vector<std::vector<int>> rising_subsequences;
        std::vector<int> current_subsequence;
        current_subsequence.push_back(arr[0]);

        for(int i = 1; i < arr.size(); ++i)
        {
            if(arr[i] > arr[i - 1])
            {
                current_subsequence.push_back(arr[i]);
            }
            else
            {
                rising_subsequences.push_back(current_subsequence);
                current_subsequence.clear();
                current_subsequence.push_back(arr[i]);
            }
        }

        rising_subsequences.push_back(current_subsequence);
        return rising_subsequences;
    };

    std::function<void(std::vector<int>&, std::vector<std::vector<int>>)> merge_rising_subsequences = [&](std::vector<int>& arr, std::vector<std::vector<int>> rising_subsequences)
    {
        std::vector<int> tmp(arr.size());
        int k = 0;

        for(int i = 0; i < rising_subsequences.size(); ++i)
        {
            for(int j = 0; j < rising_subsequences[i].size(); ++j)
            {
                tmp[k++] = rising_subsequences[i][j];
            }
        }

        for(int i = 0; i < arr.size(); ++i)
        {
            arr[i] = tmp[i];
        }
    };
    // doesnt work at all..
    merge_rising_subsequences(arr, find_rising_subsequences(arr));
}

// global map of sorts
const std::unordered_map<std::string, std::function<void(std::vector<int>&)>> sort_map = 
{
    {"std", std_sort},
    {"insertion", insertion_sort},
    {"quick", quick_sort},
    {"dpquick", dual_pivot_quick_sort},
    {"hybrid", hybrid_sort},
    {"altmerge", alt_merge_sort},
    {"merge", merge_sort}
};

int main(int argc, char* argv[]) 
{
    if (argc != 2) 
    {
        std::cerr << "Usage: " << argv[0] << " <sort_type>\n";
        return 1;
    }
    
    std::string sort_type = argv[1];
    std::vector<int> numbers;
    int num;
    
    while (std::cin >> num) 
    {
        numbers.push_back(num);
    }

    std::vector<int> numbers_copy = numbers;
    
    try 
    {
        sort_map.at(sort_type)(numbers);
    } 
    catch (const std::out_of_range&) 
    {
        std::cerr << "Invalid sort type [" << sort_type << "]" << std::endl;
        return 1;
    }

    std::cout << "Before sorting: ";
    for (int num : numbers_copy) 
    {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    std::cout << "After sorting: ";
    for (int num : numbers) 
    {
        std::cout << num << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
