#include "BinHeap.hpp"
#include <cmath>
#include <climits>
#include <vector>
#include <stdexcept>
#include <utility>
#include <algorithm>
// ──────────────────────────── BinomialNode ────────────────────────────
BinomialNode::BinomialNode(int val)
    : value(val), degree(0), parent(nullptr), children() {}

// ──────────────────────────── BinomialHeap ────────────────────────────
// Constructor for the Binomial Heap
BinomialHeap::BinomialHeap() {
    min_node = nullptr;
    count = 0;
    trees.clear();
}

// Set metrics for the heap operations
void BinomialHeap::setMetrics(Metrics& metrics) {
    this->metrics = &metrics;
}

// Check if the heap is empty
bool BinomialHeap::is_empty() {
    return min_node == nullptr;
}

// Insert a new value into the heap
void BinomialHeap::insert(int value) {
    BinomialNode* node = new BinomialNode(value);
    BinomialHeap heap;
    heap.trees.push_back(node);
    merge(heap);
}

// Get the minimum value in the heap
int BinomialHeap::get_min() {
    return min_node->value;
}

// Extract the minimum value from the heap
int BinomialHeap::extract_min() {
    BinomialNode* minNode = min_node;
    trees.erase(remove(trees.begin(), trees.end(), minNode), trees.end());
    BinomialHeap heap;
    heap.trees = minNode->children;
    merge(heap);
    _find_min();
    count -= 1;
    return minNode->value;
}

// Merge two binomial heaps
void BinomialHeap::merge(BinomialHeap& other_heap) {
    trees.insert(trees.end(), other_heap.trees.begin(), other_heap.trees.end());
    count += other_heap.count;
    _find_min();
}

// Find the minimum value in the heap
void BinomialHeap::_find_min() {
    min_node = nullptr;
    for (BinomialNode* tree : trees) {
        if(this -> metrics != nullptr) { this -> metrics -> comparisons++; }
        if (min_node == nullptr || tree->value < min_node->value) {
            min_node = tree;
        }
    }
}

// Decrease the key of a node
void BinomialHeap::decrease_key(BinomialNode* node, int new_value) {
    if (new_value > node->value) {
        throw std::invalid_argument("New value is greater than the current value");
    }
    node->value = new_value;
    _bubble_up(node);
}

// Delete a specific node from the heap
void BinomialHeap::delete_node(BinomialNode* node) {
    decrease_key(node, INT_MIN);
    extract_min();
}

// Perform the bubbling up operation
void BinomialHeap::_bubble_up(BinomialNode* node) {
    BinomialNode* parent = node->parent;
    while (parent != nullptr && node->value < parent->value) {
        if(this -> metrics != nullptr) { this -> metrics -> comparisons++; }
        std::swap(node->value, parent->value);
        node = parent;
        parent = node->parent;
    }
}

// Link two trees together
void BinomialHeap::_link(BinomialNode* tree1, BinomialNode* tree2) {
    if(this -> metrics != nullptr) { this -> metrics -> comparisons++; }
    if (tree1->value > tree2->value) {
        std::swap(tree1, tree2);
    }
    tree2->parent = tree1;
    tree1->children.push_back(tree2);
    tree1->degree += 1;
}

// Consolidate the trees in the heap
void BinomialHeap::_consolidate() {
    int max_degree = static_cast<int>(floor(log2(count))) + 1;
    std::vector<BinomialNode*> degree_to_tree(max_degree + 1, nullptr);

    while (!trees.empty()) {
        BinomialNode* current = trees[0];
        trees.erase(trees.begin());
        int degree = current->degree;
        while (degree_to_tree[degree] != nullptr) {
            BinomialNode* other = degree_to_tree[degree];
            degree_to_tree[degree] = nullptr;
            if(this -> metrics != nullptr) { this -> metrics -> comparisons++; }
            if (current->value < other->value) {
                _link(current, other);
            } else {
                _link(other, current);
                current = other;
            }
            degree++;
        }
        degree_to_tree[degree] = current;
    }

    min_node = nullptr;
    trees.clear();
    for (BinomialNode* tree : degree_to_tree) {
        if (tree != nullptr) {
            trees.push_back(tree);
            if(this -> metrics != nullptr) { this -> metrics -> comparisons++; }
            if (min_node == nullptr || tree->value < min_node->value) {
                min_node = tree;
            }
        }
    }
}

// Get the size of the heap
int BinomialHeap::size() {
    return count;
}

