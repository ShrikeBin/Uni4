#include "schedule.hpp"
#include <vector>
#include <algorithm>
#include <functional>
#include "prim.hpp"

int computeBroadcastRounds(int n, int root, std::vector<Edge>& MSTedges, std::vector<int>& rounds) {

    // Build adjacency list from MST edges
    std::vector<std::vector<int>> adj(n);
    for (auto& [u, v, weigh] : MSTedges) {
        adj[u].push_back(v);
        adj[v].push_back(u);
    }

    // DP broadcast rounds using your adjacency
    std::function<int(int, int)> dfs = [&](int node, int parent) -> int {
        std::vector<int> childrenTimes;
        for (int child : adj[node]) {
            if (child == parent) continue; // Avoid going back to parent
            childrenTimes.push_back(dfs(child, node));
        }
        std::sort(childrenTimes.begin(), childrenTimes.end(), std::greater<int>());
        int maxTime = 0;
        for (int i = 0; i < (int)childrenTimes.size(); i++) {
            maxTime = std::max(maxTime, childrenTimes[i] + i + 1);
        }
        rounds[node] = maxTime;
        return maxTime;
    };

    return dfs(root, -1);
}

