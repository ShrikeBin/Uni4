#include "exp.hpp"
#include <iostream>
#include <fstream>
#include <random>
#include <algorithm>

std::vector<int> generate_random(int n) 
{
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<int> dist(0, 2 * n - 1);
    std::vector<int> vec(n);
    
    for (int i = 0; i < n; ++i) {
        vec[i] = dist(mt);
    }
    return vec;
}

std::vector<int> generate_ascending(int n) 
{
    std::vector<int> vec(n);
    for (int i = 0; i < n; ++i) 
    {
        vec[i] = i;
    }
    return vec;
}

std::vector<int> generate_descending(int n) 
{
    std::vector<int> vec(n);
    for (int i = n - 1; i >= 0; --i) 
    {
        vec[n - 1 - i] = i;
    }
    return vec;
}

void write_stats_to_file(const std::string& filename, const Stats& stats, int n, std::string mode, double time) 
{
    std::ofstream file(filename, std::ios::app);
    file << "N: " << n << " mode: " << mode << std::endl;
    file << "Avg Time: " << time << " sec." << std::endl;
    file << "Comparisons: " << stats.comparisons << std::endl;
    file << "Swaps: " << stats.swaps << std::endl;
    file << "Comparisons/n: " << (stats.comparisons / static_cast<double>(n)) << std::endl;
    file << "Swaps/n: " << (stats.swaps / static_cast<double>(n)) << std::endl;
    file << "-----" <<std::endl;
}


void run_experiment(int n, int k, const std::string& mode, const std::unordered_map<std::string, std::function<void(std::vector<int>&, Stats&)>>& sort_map) 
{
    std::vector<int> data;
    
    for (const auto& entry : sort_map) 
    {
        const std::string& sort_type = entry.first;
        const auto& sort_func = entry.second;
        
        Stats total_stats;
        double total_time = 0.0;

        if (mode == "random") 
        {
            for (int i = 0; i < k; ++i) 
            {
                data = generate_random(n);
                std::vector<int> numbers = data;
                Stats stats;

                auto start_time = std::chrono::high_resolution_clock::now();

                sort_func(numbers, stats);

                auto end_time = std::chrono::high_resolution_clock::now();
                std::chrono::duration<double> elapsed_time = end_time - start_time;
                total_time += elapsed_time.count();

                total_stats.comparisons += stats.comparisons;
                total_stats.swaps += stats.swaps;
            }

            total_stats.comparisons /= k;
            total_stats.swaps /= k;
            total_time /= k;
        }
        else if (mode == "asc") 
        {
            data = generate_ascending(n);

            std::vector<int> numbers = data;
            Stats stats;

            auto start_time = std::chrono::high_resolution_clock::now();

            sort_func(numbers, stats);

            auto end_time = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double> elapsed_time = end_time - start_time;
            total_time += elapsed_time.count();

            total_stats.comparisons += stats.comparisons;
            total_stats.swaps += stats.swaps;
        } 
        else if (mode == "desc") 
        {
            data = generate_descending(n);

            std::vector<int> numbers = data;
            Stats stats;

            auto start_time = std::chrono::high_resolution_clock::now();

            sort_func(numbers, stats);

            auto end_time = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double> elapsed_time = end_time - start_time;
            total_time += elapsed_time.count();

            total_stats.comparisons += stats.comparisons;
            total_stats.swaps += stats.swaps;
        }

        write_stats_to_file("./results/" + sort_type + "_stats" + std::to_string(k) + ".txt", total_stats, n, mode, total_time);
    }
}

void run_full_experiment(int k, const std::unordered_map<std::string, std::function<void(std::vector<int>&, Stats&)>>& sort_map) 
{
    for(int n = 1000; n < 51000; n += 1000)
    {
        run_experiment(n, k, "random", sort_map);
        run_experiment(n, k, "asc", sort_map);
        run_experiment(n, k, "desc", sort_map);
    }
}

