#include <iostream>
#include <vector>

void DPCQsort_impl (std::vector<int>& arr, int left, int right) 
{
    if (left >= right) return;

    //  pivots
    int p = arr[left];
    int q = arr[right];

    int i = left + 1;
    int k = right - 1;
    int j = i;
    int d = 0; // |small| - |large|

    while (j <= k)
    {
        if (d > 0)
        {
            if (arr[j] <= p)
            {
                std::swap(arr[i], arr[j]);
                ++i;
                ++j;
                ++d;
            }
            else
            {
                if (arr[j] < q)
                {
                    ++j;
                }
                else
                {
                    std::swap(arr[j], arr[k]);
                    --k;
                    --d;
                }
            }
        }
        else
        {
            while (arr[k] > q)
            {
                --k;
                --d;
            }

            if(j <= k)
            {
                if (arr[k] <= p)
                {
                    // rotate3(arr[k], arr[j], arr[i]);
                    int temp = arr[k];
                    arr[k] = arr[j];
                    arr[j] = arr[i];
                    arr[i] = temp;
                    ++i;
                    ++d;
                }
                else
                {
                    std::swap(arr[j], arr[k]);
                }
                ++j;
            }
        }
    }

    std::swap(arr[left], arr[i - 1]);
    std::swap(arr[right], arr[k + 1]);
}

int main() {
    std::vector<int> arr = {3, 1, 6, 3, 7, 9, 8, 4, 5};
    int left = 0, right = arr.size() - 1;
    DPCQsort_impl(arr, left, right);

    std::cout << "Partitioned array: ";
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    return 0;
}
