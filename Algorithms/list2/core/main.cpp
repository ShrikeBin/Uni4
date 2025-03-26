#include <iostream>
#include <vector>
#include <unordered_map>
#include <functional>
#include <chrono>

#include "sorts.hpp"

const std::unordered_map<std::string, std::function<void(std::vector<int>&, SortStats&)>> sort_map = 
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
    SortStats stats;  
    
    try 
    {
        auto start = std::chrono::high_resolution_clock::now();

        sort_map.at(sort_type)(numbers, stats);

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = end - start;

        std::cout << "Time taken to sort: " << duration.count() << " seconds\n";
    } 
    catch (const std::out_of_range&) 
    {
        std::cerr << "Invalid sort type [" << sort_type << "]" << std::endl;
        std::cout << "Available sort types: " << std::endl;
        for (const auto& entry : sort_map) 
        {
            std::cout << "[" << entry.first <<"]" <<std::endl;
        }        
        return 1;
    }

    if(numbers.size() < 40) 
    {
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
    
    } 
    return 0;
}