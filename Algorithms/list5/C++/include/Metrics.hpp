#ifndef METRICS_HPP
#define METRICS_HPP

struct Metrics {
    Metrics() : comparisons(0), swaps(0), insertions(0), deletions(0) {}
    void reset() {
        comparisons = 0;
        swaps = 0;
        insertions = 0;
        deletions = 0;
    }
    long long comparisons;
    long long swaps;
    long long insertions;
    long long deletions;
};

#endif // METRICS_HPP