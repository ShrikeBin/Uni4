#include "exp.hpp"
#include <iostream>
#include <fstream>
#include <random>
#include <algorithm>
#include <chrono>
#include <vector>

#include "select.hpp"
#include "stats.hpp"
#include "sorts.hpp"
#include "search.hpp"

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
    std::ofstream file("./results/" + filename, std::ios::app);
    file << "N: " << n << " mode: " << mode << std::endl;
    file << "Avg Time: " << time << " sec." << std::endl;
    file << "Comparisons: " << stats.comparisons << std::endl;
    file << "Swaps: " << stats.swaps << std::endl;
    file << "Comparisons/n: " << (stats.comparisons / static_cast<double>(n)) << std::endl;
    file << "Swaps/n: " << (stats.swaps / static_cast<double>(n)) << std::endl;
    file << "-----" <<std::endl;
}


void run_select(int n, int repeats) 
{
    Stats total_stats_middle;
    Stats total_stats_end;
    Stats total_stats_start;

    auto start_mid = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n / 2;
        Select(data, k, stats);
        total_stats_middle.comparisons += stats.comparisons;
        total_stats_middle.swaps += stats.swaps;
    }
    auto end_mid = std::chrono::high_resolution_clock::now();

    auto start_top = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n - 10;
        Select(data, k, stats);
        total_stats_end.comparisons += stats.comparisons;
        total_stats_end.swaps += stats.swaps;
    }
    auto end_top = std::chrono::high_resolution_clock::now();

    auto start_low = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = 10;
        Select(data, k, stats);
        total_stats_start.comparisons += stats.comparisons;
        total_stats_start.swaps += stats.swaps;
    }
    auto end_low = std::chrono::high_resolution_clock::now();

    total_stats_end.comparisons /= repeats;
    total_stats_end.swaps /= repeats;
    total_stats_middle.comparisons /= repeats;
    total_stats_middle.swaps /= repeats;
    total_stats_start.comparisons /= repeats;
    total_stats_start.swaps /= repeats;

    double time_mid = std::chrono::duration<double>(end_mid - start_mid).count() / repeats;
    double time_top = std::chrono::duration<double>(end_top - start_top).count() / repeats;
    double time_low = std::chrono::duration<double>(end_low - start_low).count() / repeats;

    write_stats_to_file("select_mid.txt", total_stats_middle, n, "select middle", time_mid);
    write_stats_to_file("select_top.txt", total_stats_end, n, "select top", time_top);
    write_stats_to_file("select_low.txt", total_stats_start, n, "select low", time_low);
}


void run_rselect(int n, int repeats) 
{
    Stats total_stats_middle;
    Stats total_stats_end;
    Stats total_stats_start;

    auto start_mid = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n / 2;
        RandomSelect(data, k, stats);
        total_stats_middle.comparisons += stats.comparisons;
        total_stats_middle.swaps += stats.swaps;
    }
    auto end_mid = std::chrono::high_resolution_clock::now();

    auto start_top = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n - 10;
        RandomSelect(data, k, stats);
        total_stats_end.comparisons += stats.comparisons;
        total_stats_end.swaps += stats.swaps;
    }
    auto end_top = std::chrono::high_resolution_clock::now();

    auto start_low = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = 10;
        RandomSelect(data, k, stats);
        total_stats_start.comparisons += stats.comparisons;
        total_stats_start.swaps += stats.swaps;
    }
    auto end_low = std::chrono::high_resolution_clock::now();

    total_stats_end.comparisons /= repeats;
    total_stats_end.swaps /= repeats;
    total_stats_middle.comparisons /= repeats;
    total_stats_middle.swaps /= repeats;
    total_stats_start.comparisons /= repeats;
    total_stats_start.swaps /= repeats;

    double time_mid = std::chrono::duration<double>(end_mid - start_mid).count() / repeats;
    double time_top = std::chrono::duration<double>(end_top - start_top).count() / repeats;
    double time_low = std::chrono::duration<double>(end_low - start_low).count() / repeats;

    write_stats_to_file("randselect_mid.txt", total_stats_middle, n, "randselect middle", time_mid);
    write_stats_to_file("randselect_top.txt", total_stats_end, n, "randselect top", time_top);
    write_stats_to_file("randselect_low.txt", total_stats_start, n, "randselect low", time_low);
}

void run_pselect(int n, int repeats, int parameter) 
{
    Stats total_stats_middle;
    Stats total_stats_end;
    Stats total_stats_start;

    auto start_mid = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n / 2;
        ParametrizedSelect(data, k, parameter, stats);
        total_stats_middle.comparisons += stats.comparisons;
        total_stats_middle.swaps += stats.swaps;
    }
    auto end_mid = std::chrono::high_resolution_clock::now();

    auto start_top = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = n - 10;
        ParametrizedSelect(data, k, parameter, stats);
        total_stats_end.comparisons += stats.comparisons;
        total_stats_end.swaps += stats.swaps;
    }
    auto end_top = std::chrono::high_resolution_clock::now();

    auto start_low = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_random(n);
        int k = 10;
        ParametrizedSelect(data, k, parameter, stats);
        total_stats_start.comparisons += stats.comparisons;
        total_stats_start.swaps += stats.swaps;
    }
    auto end_low = std::chrono::high_resolution_clock::now();

    total_stats_end.comparisons /= repeats;
    total_stats_end.swaps /= repeats;
    total_stats_middle.comparisons /= repeats;
    total_stats_middle.swaps /= repeats;
    total_stats_start.comparisons /= repeats;
    total_stats_start.swaps /= repeats;

    double time_mid = std::chrono::duration<double>(end_mid - start_mid).count() / repeats;
    double time_top = std::chrono::duration<double>(end_top - start_top).count() / repeats;
    double time_low = std::chrono::duration<double>(end_low - start_low).count() / repeats;

    write_stats_to_file("paramselect_mid_" + std::to_string(parameter) + ".txt", total_stats_middle, n, "paramselect middle, p: " + std::to_string(parameter), time_mid);
    write_stats_to_file("paramselect_top_" + std::to_string(parameter) + ".txt", total_stats_end, n, "paramselect top, p: " + std::to_string(parameter), time_top);
    write_stats_to_file("paramselect_low_" + std::to_string(parameter) + ".txt", total_stats_start, n, "paramselect low, p: " + std::to_string(parameter), time_low);
}

void run_binsearch(int n, int repeats)  
{
    Stats total_stats_middle;
    Stats total_stats_end;
    Stats total_stats_start;
    Stats total_stats_notin;

    auto start_mid = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_ascending(n);
        int k = n / 2;
        binsearch(data, k, stats);
        total_stats_middle.comparisons += stats.comparisons;
        total_stats_middle.swaps += stats.swaps;
    }
    auto end_mid = std::chrono::high_resolution_clock::now();

    auto start_top = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_ascending(n);
        int k = n - 10;
        binsearch(data, k, stats);
        total_stats_end.comparisons += stats.comparisons;
        total_stats_end.swaps += stats.swaps;
    }
    auto end_top = std::chrono::high_resolution_clock::now();

    auto start_low = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_ascending(n);
        int k = 10;
        binsearch(data, k, stats);
        total_stats_start.comparisons += stats.comparisons;
        total_stats_start.swaps += stats.swaps;
    }
    auto end_low = std::chrono::high_resolution_clock::now();

    auto start_not = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_ascending(n);
        int k = n + 10;
        binsearch(data, k, stats);
        total_stats_notin.comparisons += stats.comparisons;
        total_stats_notin.swaps += stats.swaps;
    }
    auto end_not = std::chrono::high_resolution_clock::now();

    total_stats_end.comparisons /= repeats;
    total_stats_end.swaps /= repeats;
    total_stats_middle.comparisons /= repeats;
    total_stats_middle.swaps /= repeats;
    total_stats_start.comparisons /= repeats;
    total_stats_start.swaps /= repeats;
    total_stats_notin.comparisons /= repeats;
    total_stats_notin.swaps /= repeats;

    double time_mid = std::chrono::duration<double>(end_mid - start_mid).count() / repeats;
    double time_top = std::chrono::duration<double>(end_top - start_top).count() / repeats;
    double time_low = std::chrono::duration<double>(end_low - start_low).count() / repeats;
    double time_not = std::chrono::duration<double>(end_not - start_not).count() / repeats;

    write_stats_to_file("binsearch_mid.txt", total_stats_middle, n, "binsearch middle", time_mid);
    write_stats_to_file("binsearch_top.txt", total_stats_end, n, "binsearch top", time_top);
    write_stats_to_file("binsearch_low.txt", total_stats_start, n, "binsearch low", time_low);
    write_stats_to_file("binsearch_not.txt", total_stats_notin, n, "binsearch not in", time_not);
}

void run_quick(int n, int repeats) 
{
    Stats total_stats_normal;
    Stats total_stats_better;

    auto start_normal = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_descending(n);
        quick_sort(data, stats);
        total_stats_normal.comparisons += stats.comparisons;
        total_stats_normal.swaps += stats.swaps;
    }
    auto end_normal = std::chrono::high_resolution_clock::now();

    auto start_better = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_descending(n);
        better_quick_sort(data, stats);
        total_stats_better.comparisons += stats.comparisons;
        total_stats_better.swaps += stats.swaps;
    }
    auto end_better = std::chrono::high_resolution_clock::now();

    total_stats_normal.comparisons /= repeats;
    total_stats_normal.swaps /= repeats;
    total_stats_better.comparisons /= repeats;
    total_stats_better.swaps /= repeats;

    double time_normal = std::chrono::duration<double>(end_normal- start_normal).count() / repeats;
    double time_better = std::chrono::duration<double>(end_better - start_better).count() / repeats;

    write_stats_to_file("quick_normal.txt", total_stats_normal, n, "normal quicksort worst case", time_normal);
    write_stats_to_file("quick_better.txt", total_stats_better, n, "better quicksort worst case", time_better);
}

void run_dpquick(int n, int repeats) 
{
    Stats total_stats_normal;
    Stats total_stats_better;

    auto start_normal = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_descending(n);
        dual_pivot_quick_sort(data, stats);
        total_stats_normal.comparisons += stats.comparisons;
        total_stats_normal.swaps += stats.swaps;
    }
    auto end_normal = std::chrono::high_resolution_clock::now();

    auto start_better = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) 
    {
        Stats stats;
        auto data = generate_descending(n);
        better_dual_pivot_quick_sort(data, stats);
        total_stats_better.comparisons += stats.comparisons;
        total_stats_better.swaps += stats.swaps;
    }
    auto end_better = std::chrono::high_resolution_clock::now();

    total_stats_normal.comparisons /= repeats;
    total_stats_normal.swaps /= repeats;
    total_stats_better.comparisons /= repeats;
    total_stats_better.swaps /= repeats;

    double time_normal = std::chrono::duration<double>(end_normal- start_normal).count() / repeats;
    double time_better = std::chrono::duration<double>(end_better - start_better).count() / repeats;

    write_stats_to_file("dpquick_normal.txt", total_stats_normal, n, "normal dpquicksort worst case", time_normal);
    write_stats_to_file("dpquick_better.txt", total_stats_better, n, "better dpquicksort worst case", time_better);
}

void run_full(int repeats) 
{
    for (int i = 1000; i <= 100000; i += 1000) 
    {
        std::cout << "Running for n = " << i << std::endl;
        run_select(i, repeats);
        run_rselect(i, repeats);

        run_pselect(i, repeats, 3);
        run_pselect(i, repeats, 5);
        run_pselect(i, repeats, 10);
        run_pselect(i, repeats, 15);
        run_pselect(i, repeats, 20);
        run_pselect(i, repeats, 30);
        run_pselect(i, repeats, 50);
        run_pselect(i, repeats, 80);

        run_binsearch(i, repeats);
        run_quick(i, repeats);
        run_dpquick(i, repeats);
    }
}

