#ifndef BINHEAP_HPP
#define BINHEAP_HPP

struct BinomialNode {
    int key;
    int degree;
    BinomialNode* parent;
    BinomialNode* child;
    BinomialNode* sibling;

    BinomialNode(int k);
};

class BinomialHeap {
public:
    BinomialHeap();

    void makeHeap();
    void clear();

    void insert(int key);
    void unionHeap(BinomialHeap& other);

    int extractMin();
    bool empty() const;

    void resetComparisonCount();
    void addToTotalComparisons();
    long long getCurrentOpComparisons() const;
    long long getTotalComparisons() const;

private:
    BinomialNode* head;

    long long total_comparisons;
    long long current_op_comparisons;

    bool less_or_equal(int a, int b);
    void linkTrees(BinomialNode* y, BinomialNode* z);
    BinomialNode* mergeRootLists(BinomialNode* h1, BinomialNode* h2);
    BinomialNode* unionHeaps(BinomialNode* h1, BinomialNode* h2);
};

#endif // BINHEAP_HPP