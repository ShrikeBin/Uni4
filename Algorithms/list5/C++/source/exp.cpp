#include "exp.hpp"
#include "BinHeap.hpp"
#include <iostream>
#include <vector>
#include <random>
#include <fstream>

// Helper: generate vector of random ints size n
std::vector<int> randomSequence(int n, int seed) {
    std::mt19937 rng(seed);
    std::uniform_int_distribution<int> dist(1, 1000000);
    std::vector<int> v(n);
    for (int &x : v) x = dist(rng);
    return v;
}

void runHeapExperiment(int n, int trials) {
    std::ofstream out("results/heap_exp_results.txt");
    for (int trial = 1; trial <= trials; trial++) {
        std::cout << "Trial " << trial << ":\n";
        BinomialHeap H1, H2;

        H1 = BinomialHeap();
        H2 = BinomialHeap();

        Metrics metrics1, metrics2;
        metrics1 = Metrics();
        metrics2 = Metrics();
        H1.setMetrics(metrics1);
        H2.setMetrics(metrics2);

        auto seq1 = randomSequence(n, trial * 100);
        auto seq2 = randomSequence(n, trial * 200);

        std::vector<long long> insert_comps_H1;
        std::vector<long long> insert_comps_H2;

        for (int x : seq1) {
            H1.insert(x);
            insert_comps_H1.push_back(metrics1.comparisons);
            metrics1.reset();
        }

        for (int x : seq2) {
            H2.insert(x);
            insert_comps_H2.push_back(metrics2.comparisons);
            metrics2.reset();
        }

        metrics1.reset();
        H1.merge(H2);
        long long union_comps = metrics1.comparisons;
        metrics1.reset();
        
        std::vector<long long> extract_comps;
        std::vector<int> extracted;

        bool sorted = true;

        for (int i = 0; i < 2 * n; i++) {
            int val = H1.extract_min();
            extracted.push_back(val);
            extract_comps.push_back(metrics1.comparisons);
            metrics1.reset();
            if (i > 0 && extracted[i] < extracted[i - 1]) sorted = false;
        }

        bool empty_after = H1.is_empty();

        
        out << "---------------------------------------\n";
        out << "Trial " << trial << " results:\n";
        out<< "Insert Heap 1 comparisons (per insert): ";
        for (auto c : insert_comps_H1) out << c << " ";
        out << "\nInsert Heap 2 comparisons (per insert): ";
        for (auto c : insert_comps_H2) out << c << " ";
        out << "\nUnion comparisons: " << union_comps << "\n";
        out << "Extract-Min comparisons (per op): ";
        for (auto c : extract_comps) out << c << " ";
        out << "\nExtracted sequence sorted: " << (sorted ? "YES" : "NO") << "\n";
        out << "Heap empty after 2n extracts: " << (empty_after ? "YES" : "NO") << "\n";
        out << "---------------------------------------\n";
    }
}
