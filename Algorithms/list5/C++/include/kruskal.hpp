#ifndef KRUSKAL_HPP
#define KRUSKAL_HPP

#include "Graph.hpp"
#include <vector>

class DisjointSet {
public:
    DisjointSet(int n);
    int find(int x);
    void unite(int x, int y);

private:
    std::vector<int> parent, rank;
};

std::vector<Edge> kruskalMST(const Graph& graph);

#endif // KRUSKAL_HPP
