#include "cyclistd.hpp"
#include <iostream>
#include <vector>
#include <random>
#include <ctime>

const int NUM_ELEMENTS = 10000;
const int SEARCH_QUERIES = 1000;
const int RANGE = 100000;

int search(const CycListD<int>& list, int value) 
{
    int comparisons = 0;
    if (list.isEmpty()) return comparisons;

    Node<int>* current = list.front->next;
    do {
        comparisons++;
        if (current->data == value) return comparisons;
        current = current->next;
    } while (current != list.front->next);
 
    std::cout << "Value " << value << " not found after " << comparisons << " comparisons.\n";
    return comparisons;
}

void printList(const CycListD<int>& list) 
{
    if (list.isEmpty()) 
    {
        std::cout << "List is empty.\n";
        return;
    }

    Node<int>* current = list.front->next;
    do {
        std::cout << "_" << current->data;
        current = current->next;
    } while (current != list.front->next);
    std::cout << "\n";
}

int main() 
{
    std::mt19937 rng(static_cast<unsigned>(time(nullptr)));
    std::uniform_int_distribution<int> dist(0, RANGE);
    
    std::vector<int> T;
    CycListD<int> L;

    for (int i = 0; i < NUM_ELEMENTS; i++) 
    {
        int num = dist(rng);
        T.push_back(num);
        L.insert(num);
    }

    // Wyszukiwanie elementów z T
    unsigned long long totalComparisons = 0;

    for (int i = 0; i < SEARCH_QUERIES; i++) 
    {
        int value = T[rng() % NUM_ELEMENTS];
        totalComparisons += search(L, value);
    }
    std::cout << "Sredni koszt wyszukiwania elementow z T: " << (totalComparisons / static_cast<double>(SEARCH_QUERIES)) << " porownan.\n";

    // Wyszukiwanie losowych liczb z zakresu I
    totalComparisons = 0;
    
    for (int i = 0; i < SEARCH_QUERIES; i++) 
    {
        int value = dist(rng);
        totalComparisons += search(L, value);
    }
    std::cout << "Sredni koszt wyszukiwania losowych elementow z I: " << (totalComparisons / static_cast<double>(SEARCH_QUERIES)) << " porownan.\n";

    // Merge/insert
    CycListD<int> list1, list2;
    std::uniform_int_distribution<int> smallDist(10, 99);
    
    std::cout << "List 1 before insert: ";
    printList(list1);
    std::cout << "List 2 before insert: ";
    printList(list2);

    for (int i = 0; i < 10; i++) 
    {
        list1.insert(smallDist(rng));
        list2.insert(smallDist(rng));
    }
    
    std::cout << "List 1 before merge: ";
    printList(list1);
    std::cout << "List 2 before merge: ";
    printList(list2);

    list1.merge(list2);
    std::cout << "Merged list: ";
    printList(list1);

    return 0;
}
