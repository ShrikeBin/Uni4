#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <vector>
#include <tuple>
#include <random>
#include <utility>

using Edge = std::tuple<int, int, double>; // (u, v, weight)

class Graph {
public:
    Graph();
    void generateCompleteGraph(int n);
    const std::vector<Edge>& getEdges() const;
    int getVertexCount() const;

private:
    int n;
    std::vector<Edge> edges;
};

#endif // GRAPH_HPP
