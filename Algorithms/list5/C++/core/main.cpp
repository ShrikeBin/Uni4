#include "BinHeap.hpp"
#include "Metrics.hpp"
#include "exp.hpp"
#include "Graph.hpp"
#include "kruskal.hpp"
#include "prim.hpp"
#include "schedule.hpp"
#include <iostream>

int main() {
    std::cout << "Running Binomial Heap Experiment...\n";
    int n = 1000; // Size of the random sequence
    int trials = 5; // Number of trials
    //runExperiment(n, trials);

    std::cout << "Running Graph Algorithms...\n";
    Graph g;


    std::cout << "Kruskal's Algorithm:\n";
    //kruskal(g);

    std::cout << "Prim's Algorithm:\n";
    //prim(g);

    std::cout << "Running Scheduling Algorithm...\n";
    //schedule();

    return 0;
}