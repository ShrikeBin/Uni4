#include <iostream>
#include <vector>
#include <unordered_map>
#include <functional>
#include <chrono>

#include "sorts.hpp"
#include "stats.hpp"
#include "select.hpp"
#include "search.hpp"


std::unordered_map<std::string, std::function<void(std::vector<int>&, Stats&, int, int)>> op_map = {
    {"hybrid", [](std::vector<int>& arr, Stats& stats, int, int) { hybrid_sort(arr, stats); }},
    {"quick", [](std::vector<int>& arr, Stats& stats, int, int) { better_quick_sort(arr, stats); }},
    {"test", [](std::vector<int>& arr, Stats& stats, int, int) { quick_sort(arr, stats); }},
    {"dpquick", [](std::vector<int>& arr, Stats& stats, int, int) { better_dual_pivot_quick_sort(arr, stats); }},
    {"select", [](std::vector<int>& arr, Stats& stats, int k, int) { Select(arr, k, stats); }},
    {"rselect", [](std::vector<int>& arr, Stats& stats, int k, int) { RandomSelect(arr, k, stats); }},
    {"search", [](std::vector<int>& arr, Stats& stats, int target, int) { binsearch(arr, target, stats); }},
    {"pselect", [](std::vector<int>& arr, Stats& stats, int k, int partition_size) {ParametrizedSelect(arr, k, partition_size, stats); }},
};



int main(int argc, char* argv[]) 
{
    if (argc < 2) 
    {
        std::cerr << "Usage: " << argv[0] << " <operation type>\n";
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
    Stats stats;  
    
    try 
    {
        int k = 0;
        int partition_size = 0;
        if (sort_type == "select" || sort_type == "rselect" || sort_type == "search" || sort_type == "pselect") 
        {
            k = std::stoi(argv[2]);
            if (sort_type == "pselect") 
            {   
                partition_size = std::stoi(argv[3]);
                if (partition_size <= 0) 
                {
                    std::cerr << "Partition size must be greater than 0." << std::endl;
                    return 1;
                }
            }
        }

        auto start = std::chrono::high_resolution_clock::now();

        op_map.at(sort_type)(numbers, stats, k, partition_size);

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = end - start;

        std::cout << "Time taken to do operation: " << duration.count() << " seconds\n";
        std::cout << "Number of comparisons: " << stats.comparisons << "\n";
        std::cout << "Number of swaps: " << stats.swaps << "\n";
        std::cout << "Comparisons/n: " << (static_cast<double>(stats.comparisons) / numbers.size()) << "\n";
        std::cout << "Swaps/n: " << (static_cast<double>(stats.swaps) / numbers.size()) << "\n";
        std::cout << "Selects run: " << stats.selects << "\n";
    } 
    catch (const std::out_of_range&) 
    {
        std::cerr << "Invalid operation type [" << sort_type << "]" << std::endl;
        std::cout << "Available operation type: " << std::endl;
        for (const auto& entry : op_map) 
        {
            std::cout << "[" << entry.first <<"]" <<std::endl;
        }        
        return 1;
    }

    return 0;
}