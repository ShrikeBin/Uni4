#include "BinHeap.hpp"
#include "Metrics.hpp"
#include "exp.hpp"
#include "Graph.hpp"
#include "kruskal.hpp"
#include "prim.hpp"
#include "schedule.hpp"
#include <iostream>
#include <thread>

int main() {
    int n = 1000;
    int step = 500;
    int limit = 10000;
    int trials = 5;
    int rootChoices = 100;

    std::cout << "Running all experiments concurrently...\n";

    // Threads for each experiment
    std::thread heapThread([=]() {
        std::cout << "[Heap] Running Binomial Heap Experiment...\n";
        runHeapExperiment(n, trials);
        std::cout << "[Heap] Done.\n";
    });

    std::thread kruskalThread([=]() {
        std::cout << "[Kruskal] Running Kruskal's Algorithm...\n";
        runGraphExperiment(step, limit, trials, true);
        std::cout << "[Kruskal] Done.\n";
    });

    std::thread primThread([=]() {
        std::cout << "[Prim] Running Prim's Algorithm...\n";
        runGraphExperiment(step, limit, trials, false);
        std::cout << "[Prim] Done.\n";
    });

    std::thread schedulingThread([=]() {
        std::cout << "[Scheduling] Running Scheduling Algorithm...\n";
        runSchedulingExperiment(1000, 10000, 1, 100); // hardcoded here
        std::cout << "[Scheduling] Done.\n";
    });

    // Wait for all threads to complete
    heapThread.join();
    kruskalThread.join();
    primThread.join();
    schedulingThread.join();

    std::cout << "\nAll experiments completed.\n";
    std::cout << "Results saved to 'results/' directory.\n";
    return 0;
}
