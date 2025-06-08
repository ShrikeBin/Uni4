#include "exp.hpp"
#include "BinHeap.hpp"
#include "Graph.hpp"
#include "kruskal.hpp"
#include "prim.hpp"
#include "schedule.hpp"
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

#include <fstream>
#include <iostream>
#include <filesystem>

void runGraphExperiment(int step, int limit, int trials, bool useKruskal) {
    std::string file = useKruskal ? "results/kruskal_exp_results.txt" : "results/prim_exp_results.txt";
    std::ofstream out(file);
    if (!out.is_open()) {
        std::cerr << "Failed to open output file!\n";
        return;
    }
    for (int n = step; n <= limit; n += step) {
        std::cout << "  n = " << n << "...\n";
        out << "n = " << n << ":\n";

        for (int trial = 1; trial <= trials; ++trial) {
            std::cout << "Trial " << trial << "\n";
            out << "Trial " << trial << "\n";

            Graph graph;
            graph.generateCompleteGraph(n);

            std::string time;
            std::vector<Edge> MSTedges = useKruskal ? kruskalMST(graph, time) : primMST(graph, time);

            out << (useKruskal ? "Kruskal" : "Prim") << " MST computed: " << time << "\n";
        }
        out.flush();
    }

    out.flush();
}


void runSchedulingExperiment(int step, int limit, int trials, int rootChoices) {
    std::ofstream out("results/scheduling_exp_results.txt");
    if (!out.is_open()) {
        std::cerr << "Failed to open output file!\n";
        return;
    }
    std::mt19937 rng(std::random_device{}());  // move rng outside loops

    for (int n = step; n <= limit; n += step) {
        std::cout << "  n = " << n << "...\n";
        out << "n = " << n << ":\n";

        for (int trial = 1; trial <= trials; ++trial) {
            std::cout << "Trial " << trial << "\n";
            out << "Trial " << trial << "\n";

            Graph graph;
            graph.generateCompleteGraph(n);
            std::string time;
            std::vector<Edge> MSTedges = primMST(graph, time);
            std::cout << "MST computed: " << time << "\n";
            std::uniform_int_distribution<int> dist(0, n - 1);

            for (int j = 0; j < rootChoices; j++) {
                int root = dist(rng);
                std::vector<int> rounds(n, 0);
                int result = computeBroadcastRounds(n, root, MSTedges, rounds);
                out << j+1 << "th — Min rounds: " << result << "\n";
            }
            out.flush();
        }
    }
}


void runHeapExperiment(int n, int trials) {
    std::ofstream out("results/heap_exp_results.txt");
    if (!out.is_open()) {
        std::cerr << "Failed to open output file!\n";
        return;
    }
    int step = n*2;
    int our_n = n;
    while (our_n <= 50 * n)
    {
        out << "N == " << our_n << "\n";
        for (int trial = 1; trial <= trials; trial++) {
            std::cout << "Trial " << trial << "\n";
            BinomialHeap H1, H2;

            H1 = BinomialHeap();
            H2 = BinomialHeap();

            Metrics metrics1, metrics2;
            metrics1 = Metrics();
            metrics2 = Metrics();
            H1.setMetrics(metrics1);
            H2.setMetrics(metrics2);

            auto seq1 = randomSequence(our_n, trial * 100);
            auto seq2 = randomSequence(our_n, trial * 200);

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

            for (int i = 0; i < 2 * our_n; i++) {
                int val = H1.extract_min();
                extracted.push_back(val);
                extract_comps.push_back(metrics1.comparisons);
                metrics1.reset();
                if (i > 0 && extracted[i] < extracted[i - 1]) sorted = false;
            }

            bool empty_after = H1.is_empty();

            double avg_H1 = std::accumulate(insert_comps_H1.begin(), insert_comps_H1.end(), 0.0) / insert_comps_H1.size();
            double avg_H2 = std::accumulate(insert_comps_H2.begin(), insert_comps_H2.end(), 0.0) / insert_comps_H2.size();
            double avg_ex = std::accumulate(extract_comps.begin(), extract_comps.end(), 0.0) / extract_comps.size();

            out << "CSV,"
            << our_n << ','           // heap size
            << trial << ','       // trial #
            << avg_H1 << ','
            << avg_H2 << ','
            << union_comps << ','
            << avg_ex << '\n';
        }
        our_n += step;
    }
}
