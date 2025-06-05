#ifndef BINHEAP_HPP
#define BINHEAP_HPP
#include <vector>
#include "Metrics.hpp"
struct BinomialNode {
    int value;
    BinomialNode* parent;
    std::vector<BinomialNode*> children;
    int degree;
    bool marked;

    BinomialNode(int val);
};

class BinomialHeap {
public:
    std::vector<BinomialNode*> trees;
    BinomialNode* min_node;
    int count;
    Metrics* metrics;

    // Constructor for the Binomial Heap
    BinomialHeap();
    // Check if the heap is empty
    bool is_empty();
    // Insert a new value into the heap
    void insert(int value);
    // Get the minimum value in the heap
    int get_min();
    // Extract the minimum value from the heap
    int extract_min();
    // Merge two binomial heaps
    void merge(BinomialHeap& other_heap);
    // Get the size of the heap
    int size();
    // sets metrics for the heap operations
    void setMetrics(Metrics& metrics);
private:
    // Decrease the key of a node
    void decrease_key(BinomialNode* node, int new_value);
    // Delete a specific node from the heap
    void delete_node(BinomialNode* node);
    // Perform the bubbling up operation
    void _bubble_up(BinomialNode* node);
    // Link two trees together
    void _link(BinomialNode* tree1, BinomialNode* tree2);
    // Consolidate the trees in the heap
    void _consolidate();
    // Find the minimum value in the heap
    void _find_min();
};



#endif // BINHEAP_HPP