#include "prim.hpp"
#include <queue>
#include <functional>
#include <limits>
#include <chrono>

std::vector<Edge> primMST(const Graph& graph, std::string& time) {
    auto start = std::chrono::high_resolution_clock::now();
    int n = graph.getVertexCount();
    std::vector<std::vector<std::pair<int, double>>> adj(n);

    // Build adjacency list from edges (undirected)
    for (const auto& [u, v, w] : graph.getEdges()) {
        adj[u].emplace_back(v, w);
        adj[v].emplace_back(u, w);
    }

    std::vector<bool> inMST(n, false);
    std::vector<Edge> mst;
    using P = std::pair<double, std::pair<int, int>>; // weight, (from, to)
    std::priority_queue<P, std::vector<P>, std::greater<P>> pq;

    inMST[0] = true;
    for (auto& [to, w] : adj[0]) {
        pq.push({w, {0, to}});
    }

    while (!pq.empty() && mst.size() < n - 1) {
        auto [w, edge] = pq.top();
        pq.pop();
        int u = edge.first, v = edge.second;

        if (inMST[v]) continue;

        inMST[v] = true;
        mst.push_back({u, v, w});

        for (auto& [to, wt] : adj[v]) {
            if (!inMST[to]) {
                pq.push({wt, {v, to}});
            }
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    time = "Duration: " + std::to_string(elapsed.count()) + " sec";

    return mst;
}
