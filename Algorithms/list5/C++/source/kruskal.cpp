#include "kruskal.hpp"
#include <algorithm>
#include <chrono>

DisjointSet::DisjointSet(int n) : parent(n), rank(n, 0) {
    for (int i = 0; i < n; i++) parent[i] = i;
}

int DisjointSet::find(int x) {
    if (parent[x] != x)
        parent[x] = find(parent[x]);
    return parent[x];
}

void DisjointSet::unite(int x, int y) {
    int rx = find(x);
    int ry = find(y);
    if (rx != ry) {
        if (rank[rx] < rank[ry]) parent[rx] = ry;
        else if (rank[ry] < rank[rx]) parent[ry] = rx;
        else {
            parent[ry] = rx;
            rank[rx]++;
        }
    }
}

std::vector<Edge> kruskalMST(const Graph& graph, std::string& time) {
    auto start = std::chrono::high_resolution_clock::now();

    std::vector<Edge> edges = graph.getEdges();
    std::sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b) {
        return std::get<2>(a) < std::get<2>(b);
    });

    DisjointSet ds(graph.getVertexCount());
    std::vector<Edge> mst;

    for (const auto& [u, v, w] : edges) {
        if (ds.find(u) != ds.find(v)) {
            ds.unite(u, v);
            mst.push_back({u, v, w});
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    time = "Duration: " + std::to_string(elapsed.count()) + " sec";

    return mst;
}
