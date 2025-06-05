#include "BinHeap.hpp"
#include "Metrics.hpp"
#include "exp.hpp"
#include "Graph.hpp"
#include "kruskal.hpp"
#include "prim.hpp"
#include "schedule.hpp"
#include <iostream>

int main() {
    int n = 1000;
    int step = 1000;
    int limit = 100000;
    int trials = 20;

    std::cout << "Running Binomial Heap Experiment:\n";
    runHeapExperiment(n, trials);

    std::cout << "Running MST Graph Algorithms:\n";
    std::cout << "Kruskal's Algorithm:\n";
    //runGraphExperiment(step, limit, trials, true);
    std::cout << "(Not yet implemented)\n";
    
    std::cout << "Prim's Algorithm:\n";
    //runGraphExperiment(step, limit, trials, false);
    std::cout << "(Not yet implemented)\n";

    std::cout << "Running Scheduling Algorithm:\n";
    //runSchedulingExperiment(step, limit, trials);
    std::cout << "(Not yet implemented)\n";

    return 0;
}