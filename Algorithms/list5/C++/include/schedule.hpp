#ifndef SCHEDULE_HPP
#define SCHEDULE_HPP

#include <vector>
#include <tuple>

using Edge = std::tuple<int, int, double>; // MST edges: (u, v, weight)

// Computes minimal rounds to broadcast from root in a tree.
// n: number of vertices (0 to n-1)
// mstEdges: vector of edges (u, v, weight) representing the MST
// root: starting vertex
// rounds: output vector of size n, rounds[i] = rounds needed for subtree at i
// Returns total rounds needed from root to inform all.
int computeBroadcastRounds(int n, const std::vector<Edge>& mstEdges, int root, std::vector<int>& rounds);

#endif // SCHEDULE_HPP
