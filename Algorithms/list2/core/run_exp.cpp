#include "exp.hpp"
#include <iostream>
#include <unordered_map>

int main(int argc, char* argv[]) 
{
    if (argc != 2) 
    {
        std::cerr << "Usage: " << argv[0] << " <k>\n";
        return 1;
    }

    int k = std::stoi(argv[1]);


    const std::unordered_map<std::string, std::function<void(std::vector<int>&, SortStats&)>> sort_map = 
    {
        {"quick", quick_sort},
        {"dpquick", dual_pivot_quick_sort},
        {"hybrid", hybrid_sort},
        {"altmerge", alt_merge_sort},
        {"merge", merge_sort},
        {"dpcquick", DPCQsort}
    };

    run_full_experiment(k, sort_map);
    return 0;
}
