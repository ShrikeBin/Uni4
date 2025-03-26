#include <iostream>
#include <vector>

void DPCQsort_impl (std::vector<int>& arr, int left, int right) 
{
    if (left >= right) return;

        //  pivots
        int p = left;
        int q = right;

        // rest of the variables
        int pPos = left + 1;
        int qPos = right - 1;
        int i = pPos;
        int d = 0; // |small| - |large|

        while (i <= qPos) 
        {
            if (d > 0) 
            {
               if(arr[i] < arr[p]) 
               {
                   std::swap(arr[pPos], arr[i]);
                   ++pPos;
                   ++i;
                   ++d;
               } 
               else 
               {
                   if(arr[i] < arr[q]) 
                   {
                       ++i;
                   } 
                   else // wrooong
                   {
                       std::swap(arr[i], arr[qPos - 1]);
                       --qPos;
                       --d;
                   }
               }
            } 
            else // d <= 0
            {
                if(arr[i] > arr[q]) 
                {
                    std::swap(arr[i], arr[qPos]);
                    --qPos;
                    --d;
                } 
                else 
                {
                    if(arr[i] < arr[p]) 
                    {
                        std::swap(arr[pPos], arr[i]);
                        ++pPos;
                        ++i;
                        ++d;
                    } 
                    else // worg
                    {
                        std::swap(arr[i], arr[qPos - 1]);
                        --qPos;
                        --d;
                    }
                }
            }
        }

        std::swap(arr[left], arr[pPos - 1]);
        std::swap(arr[right], arr[qPos + 1]);

        std::cout << "p: " << arr[pPos - 1] << " q: " << arr[qPos + 1] << std::endl;
}

int main() {
    std::vector<int> arr = {3,1,6,2,7,9,8,4,5};
    int left = 0, right = arr.size() - 1;
    DPCQsort_impl(arr, left, right);

    std::cout << "Partitioned array: ";
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    return 0;
}
