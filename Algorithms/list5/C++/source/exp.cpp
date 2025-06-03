#include "exp.hpp"
#include "BinHeap.hpp"
#include <iostream>
#include <vector>
#include <random>

// Helper: generate vector of random ints size n
std::vector<int> randomSequence(int n, int seed) {
    std::mt19937 rng(seed);
    std::uniform_int_distribution<int> dist(1, 1000000);
    std::vector<int> v(n);
    for (int &x : v) x = dist(rng);
    return v;
}

void runExperiment(int n, int trials) {
    for (int trial = 1; trial <= trials; trial++) {
        std::cout << "Trial " << trial << ":\n";
        BinomialHeap H1, H2;

        H1.makeHeap();
        H2.makeHeap();

        auto seq1 = randomSequence(n, trial * 100);
        auto seq2 = randomSequence(n, trial * 200);

        std::vector<long long> insert_comps_H1;
        std::vector<long long> insert_comps_H2;

        for (int x : seq1) {
            H1.insert(x);
            insert_comps_H1.push_back(H1.getCurrentOpComparisons());
        }

        for (int x : seq2) {
            H2.insert(x);
            insert_comps_H2.push_back(H2.getCurrentOpComparisons());
        }

        H1.resetComparisonCount();
        H1.unionHeap(H2);
        long long union_comps = H1.getCurrentOpComparisons();

        std::vector<long long> extract_comps;
        std::vector<int> extracted;

        bool sorted = true;

        for (int i = 0; i < 2 * n; i++) {
            int val = H1.extractMin();
            extracted.push_back(val);
            extract_comps.push_back(H1.getCurrentOpComparisons());
            if (i > 0 && extracted[i] < extracted[i - 1]) sorted = false;
        }

        bool empty_after = H1.empty();

        std::cout << "Insert H1 comparisons (per insert): ";
        for (auto c : insert_comps_H1) std::cout << c << " ";
        std::cout << "\nInsert H2 comparisons (per insert): ";
        for (auto c : insert_comps_H2) std::cout << c << " ";
        std::cout << "\nUnion comparisons: " << union_comps << "\n";
        std::cout << "Extract-Min comparisons (per op): ";
        for (auto c : extract_comps) std::cout << c << " ";
        std::cout << "\nExtracted sequence sorted: " << (sorted ? "YES" : "NO") << "\n";
        std::cout << "Heap empty after 2n extracts: " << (empty_after ? "YES" : "NO") << "\n";
        std::cout << "---------------------------------------\n";
    }
}
