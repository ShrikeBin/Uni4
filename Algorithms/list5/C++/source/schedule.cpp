#include "schedule.hpp"
#include <vector>
#include <algorithm>

// Build adjacency list and directed children lists from MST edges
void buildTree(int n, const std::vector<Edge>& mstEdges, int root, std::vector<std::vector<int>>& children) {
    std::vector<std::vector<int>> adj(n);
    for (const auto& e : mstEdges) {
        int u = std::get<0>(e);
        int v = std::get<1>(e);
        adj[u].push_back(v);
        adj[v].push_back(u);
    }
    
    children.assign(n, {});
    std::vector<bool> visited(n, false);
    
    // DFS to establish parent-child
    std::function<void(int)> dfs = [&](int u) {
        visited[u] = true;
        for (int v : adj[u]) {
            if (!visited[v]) {
                children[u].push_back(v);
                dfs(v);
            }
        }
    };
    
    dfs(root);
}

// Post-order compute rounds needed for each subtree
int dfsCompute(const std::vector<std::vector<int>>& children, int u, std::vector<int>& rounds) {
    std::vector<int> childRounds;
    for (int v : children[u]) {
        int cr = dfsCompute(children, v, rounds);
        childRounds.push_back(cr);
    }
    
    std::sort(childRounds.rbegin(), childRounds.rend());
    int maxRounds = 0;
    for (int i = 0; i < (int)childRounds.size(); ++i) {
        maxRounds = std::max(maxRounds, childRounds[i] + i + 1);
    }
    rounds[u] = maxRounds;
    return maxRounds;
}

int computeBroadcastRounds(int n, const std::vector<Edge>& mstEdges, int root, std::vector<int>& rounds) {
    std::vector<std::vector<int>> children;
    buildTree(n, mstEdges, root, children);
    rounds.assign(n, 0);
    return dfsCompute(children, root, rounds);
}
