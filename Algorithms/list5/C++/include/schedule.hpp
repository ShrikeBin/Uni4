#ifndef SCHEDULE_HPP
#define SCHEDULE_HPP

#include <vector>
#include <tuple>
#include "Graph.hpp"

using Edge = std::tuple<int, int, double>; // MST edges: (u, v, weight)

// Computes the minimal number of rounds needed to broadcast a message starting from `root` in a tree.
// Parameters:
//   n      - number of vertices in the graph
//   root   - starting vertex for the broadcast
//   MSTedges - edges of the Minimum Spanning Tree (MST) of the graph
//   rounds - output vector of size n, where rounds[i] stores the minimal rounds needed to inform the subtree rooted at vertex i
//   Returns:
//   The total number of rounds needed to inform all vertices starting from `root`.
int computeBroadcastRounds(int n, int root, std::vector<Edge>& MSTedges, std::vector<int>& rounds);


#endif // SCHEDULE_HPP
