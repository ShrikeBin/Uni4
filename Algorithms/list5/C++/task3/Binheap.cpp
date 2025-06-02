#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <cassert>

struct BinomialNode {
    int key;
    int degree;
    BinomialNode* parent;
    BinomialNode* child;
    BinomialNode* sibling;

    BinomialNode(int k) : key(k), degree(0), parent(nullptr), child(nullptr), sibling(nullptr) {}
};

class BinomialHeap {
private:
    BinomialNode* head;
    // Total comparisons for all ops and current op count
    long long total_comparisons = 0;
    long long current_op_comparisons = 0;

    // Compare keys and count comparisons
    bool less_or_equal(int a, int b) {
        current_op_comparisons++;
        return a <= b;
    }

    // Link two binomial trees of the same degree
    void linkTrees(BinomialNode* y, BinomialNode* z) {
        y->parent = z;
        y->sibling = z->child;
        z->child = y;
        z->degree++;
    }

    // Merge root lists of two heaps by degree ascending
    BinomialNode* mergeRootLists(BinomialNode* h1, BinomialNode* h2) {
        if (!h1) return h2;
        if (!h2) return h1;

        BinomialNode* head = nullptr;
        BinomialNode** pos = &head;

        while (h1 && h2) {
            if (h1->degree < h2->degree) {
                *pos = h1;
                h1 = h1->sibling;
            } else {
                *pos = h2;
                h2 = h2->sibling;
            }
            pos = &((*pos)->sibling);
        }

        *pos = (h1) ? h1 : h2;
        return head;
    }

    // Union of two heaps
    BinomialNode* unionHeaps(BinomialNode* h1, BinomialNode* h2) {
        BinomialNode* newHead = mergeRootLists(h1, h2);
        if (!newHead) return nullptr;

        BinomialNode* prev = nullptr;
        BinomialNode* curr = newHead;
        BinomialNode* next = curr->sibling;

        while (next) {
            if ( (curr->degree != next->degree) ||
                 (next->sibling && next->sibling->degree == curr->degree)) {
                prev = curr;
                curr = next;
            } else {
                // Compare keys to link
                current_op_comparisons++;
                if (curr->key <= next->key) {
                    curr->sibling = next->sibling;
                    linkTrees(next, curr);
                } else {
                    if (prev) prev->sibling = next;
                    else newHead = next;
                    linkTrees(curr, next);
                    curr = next;
                }
            }
            next = curr->sibling;
        }
        return newHead;
    }

public:
    BinomialHeap() : head(nullptr) {}

    void resetComparisonCount() {
        current_op_comparisons = 0;
    }

    void addToTotalComparisons() {
        total_comparisons += current_op_comparisons;
    }

    long long getCurrentOpComparisons() const {
        return current_op_comparisons;
    }

    long long getTotalComparisons() const {
        return total_comparisons;
    }

    void clear() {
        // TODO: Implement recursive destructor for nodes if needed
        head = nullptr;
        total_comparisons = 0;
        current_op_comparisons = 0;
    }

    void makeHeap() {
        head = nullptr;
        total_comparisons = 0;
        current_op_comparisons = 0;
    }

    void insert(int key) {
        resetComparisonCount();
        BinomialNode* newNode = new BinomialNode(key);
        head = unionHeaps(head, newNode);
        addToTotalComparisons();
    }

    void unionHeap(BinomialHeap& other) {
        resetComparisonCount();
        head = unionHeaps(head, other.head);
        other.head = nullptr; // empty the other heap
        addToTotalComparisons();
    }

    bool empty() const {
        return head == nullptr;
    }

    // Extract-Min returns min key or -1 if empty
    int extractMin() {
        resetComparisonCount();
        if (!head) return -1;

        // Find min root and keep track of prev
        BinomialNode* minNode = head;
        BinomialNode* minPrev = nullptr;

        BinomialNode* curr = head;
        BinomialNode* prev = nullptr;

        while (curr) {
            current_op_comparisons++;
            if (curr->key < minNode->key) {
                minNode = curr;
                minPrev = prev;
            }
            prev = curr;
            curr = curr->sibling;
        }

        // Remove minNode from root list
        if (minPrev) {
            minPrev->sibling = minNode->sibling;
        } else {
            head = minNode->sibling;
        }

        // Reverse minNode's children list to form new heap
        BinomialNode* child = minNode->child;
        BinomialNode* newHead = nullptr;
        while (child) {
            BinomialNode* next = child->sibling;
            child->sibling = newHead;
            child->parent = nullptr;
            newHead = child;
            child = next;
        }

        head = unionHeaps(head, newHead);

        int minKey = minNode->key;
        delete minNode;

        addToTotalComparisons();
        return minKey;
    }
};

// Helper: generate vector of random ints size n
std::vector<int> randomSequence(int n, int seed) {
    std::mt19937 rng(seed);
    std::uniform_int_distribution<int> dist(1, 1000000);
    std::vector<int> v(n);
    for (int &x : v) x = dist(rng);
    return v;
}

// Experiment for n=500, 5 trials, printing comparison counts for each op
void runExperimentN500() {
    const int n = 500;
    for (int trial = 1; trial <= 20; trial++) {
        std::cout << "Trial " << trial << ":\n";
        BinomialHeap H1, H2;

        H1.makeHeap();
        H2.makeHeap();

        auto seq1 = randomSequence(n, trial * 100);
        auto seq2 = randomSequence(n, trial * 200);

        std::vector<long long> insert_comps_H1;
        std::vector<long long> insert_comps_H2;

        // Insert into H1
        for (int x : seq1) {
            H1.insert(x);
            insert_comps_H1.push_back(H1.getCurrentOpComparisons());
        }

        // Insert into H2
        for (int x : seq2) {
            H2.insert(x);
            insert_comps_H2.push_back(H2.getCurrentOpComparisons());
        }

        // Union
        H1.resetComparisonCount();
        H1.unionHeap(H2);
        long long union_comps = H1.getCurrentOpComparisons();

        // Extract min 2n times
        std::vector<long long> extract_comps;
        std::vector<int> extracted;

        bool sorted = true;
        int prev = -1;

        for (int i = 0; i < 2 * n; i++) {
            int val = H1.extractMin();
            extracted.push_back(val);
            extract_comps.push_back(H1.getCurrentOpComparisons());

            if (i > 0 && extracted[i] < extracted[i - 1]) sorted = false;
        }

        // Check empty heap after extractions
        bool empty_after = H1.empty();

        // Output summary
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

// Experiment scaling n with output of total comparisons / n (average cost per operation)
void runScalingExperiments() {
    std::cout << "N ; AVG_comparisons_per_operation\n";
    for (int n = 100; n <= 10000; n += 100) {
        BinomialHeap H1, H2;

        H1.makeHeap();
        H2.makeHeap();

        auto seq1 = randomSequence(n, n * 1000 + 1);
        auto seq2 = randomSequence(n, n * 2000 + 2);

        for (int x : seq1) H1.insert(x);
        for (int x : seq2) H2.insert(x);

        H1.unionHeap(H2);

        for (int i = 0; i < 2 * n; i++) H1.extractMin();

        // Average comparisons per operation
        long long total_ops = 2 * n + 2 * n + 1; // approx: inserts + extracts + union
        // Actually total ops: insertions (2*n), union(1), extractions(2*n) = 4n+1

        long long total_comparisons = H1.getTotalComparisons();

        double avg_comp = double(total_comparisons) / (4.0 * n + 1);

        std::cout << n << " ; " << avg_comp << "\n";
    }
}

int main() {
    // Run experiment for n=500 (5 trials)
    runExperimentN500();

    // Run scaling experiments
    runScalingExperiments();

    return 0;
}
