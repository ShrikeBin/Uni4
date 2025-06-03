#include "Graph.hpp"

Graph::Graph(int n) : n(n) {}

void Graph::generateCompleteGraph() {
    edges.clear();
    std::mt19937 rng(std::random_device{}());
    std::uniform_real_distribution<double> dist(0.0, 1.0);

    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            double weight = dist(rng);
            edges.emplace_back(i, j, weight);
        }
    }
}

const std::vector<Edge>& Graph::getEdges() const {
    return edges;
}

int Graph::getVertexCount() const {
    return n;
}
