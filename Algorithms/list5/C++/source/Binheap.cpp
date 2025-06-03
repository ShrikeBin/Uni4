#include "BinHeap.hpp"
#include <cassert>

BinomialNode::BinomialNode(int k)
    : key(k), degree(0), parent(nullptr), child(nullptr), sibling(nullptr) {}

BinomialHeap::BinomialHeap() : head(nullptr), total_comparisons(0), current_op_comparisons(0) {}

bool BinomialHeap::less_or_equal(int a, int b) {
    current_op_comparisons++;
    return a <= b;
}

void BinomialHeap::linkTrees(BinomialNode* y, BinomialNode* z) {
    y->parent = z;
    y->sibling = z->child;
    z->child = y;
    z->degree++;
}

BinomialNode* BinomialHeap::mergeRootLists(BinomialNode* h1, BinomialNode* h2) {
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

BinomialNode* BinomialHeap::unionHeaps(BinomialNode* h1, BinomialNode* h2) {
    BinomialNode* newHead = mergeRootLists(h1, h2);
    if (!newHead) return nullptr;

    BinomialNode* prev = nullptr;
    BinomialNode* curr = newHead;
    BinomialNode* next = curr->sibling;

    while (next) {
        if ((curr->degree != next->degree) ||
            (next->sibling && next->sibling->degree == curr->degree)) {
            prev = curr;
            curr = next;
        } else {
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

void BinomialHeap::resetComparisonCount() {
    current_op_comparisons = 0;
}

void BinomialHeap::addToTotalComparisons() {
    total_comparisons += current_op_comparisons;
}

long long BinomialHeap::getCurrentOpComparisons() const {
    return current_op_comparisons;
}

long long BinomialHeap::getTotalComparisons() const {
    return total_comparisons;
}

void BinomialHeap::clear() {
    head = nullptr;
    total_comparisons = 0;
    current_op_comparisons = 0;
}

void BinomialHeap::makeHeap() {
    head = nullptr;
    total_comparisons = 0;
    current_op_comparisons = 0;
}

void BinomialHeap::insert(int key) {
    resetComparisonCount();
    BinomialNode* newNode = new BinomialNode(key);
    head = unionHeaps(head, newNode);
    addToTotalComparisons();
}

void BinomialHeap::unionHeap(BinomialHeap& other) {
    resetComparisonCount();
    head = unionHeaps(head, other.head);
    other.head = nullptr;
    addToTotalComparisons();
}

bool BinomialHeap::empty() const {
    return head == nullptr;
}

int BinomialHeap::extractMin() {
    resetComparisonCount();
    if (!head) return -1;

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

    if (minPrev) {
        minPrev->sibling = minNode->sibling;
    } else {
        head = minNode->sibling;
    }

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