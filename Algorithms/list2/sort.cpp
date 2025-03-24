#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <functional>
#include <queue>

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
    // find rising subsequences
    std::function<std::vector<std::pair<int, int>>(const std::vector<int>&)> find_rising_subsequences = 
    [&](const std::vector<int>& arr) 
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

        // Merge the two sequences into tmp[]
        while (i <= leftLast && j <= rightLast) 
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

        // If there are any remaining elements in the left half, copy them over
        while (i <= leftLast) 
        {
            tmp[k++] = arr[i++];
        }

        // If there are any remaining elements in the right half, copy them over
        while (j <= rightLast) 
        {
            tmp[k++] = arr[j++];
        }

        // Copy the merged data from tmp[] back into arr[]
        for (int i = 0; i < k; ++i) 
        {
            arr[leftFirst + i] = tmp[i];
        }
    };

    // sort implementation
    std::queue<std::pair<int, int>> queue;
    std::vector<std::pair<int, int>> rising_subsequences = find_rising_subsequences(arr);
    for(std::pair<int, int> sequence : rising_subsequences) 
    {
        queue.push(sequence);
    }

    std::pair<int, int> first;
    std::pair<int, int> second;

    // bardzo misterny algorytm, ale działa xd
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
