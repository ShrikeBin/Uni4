#ifndef PRIM_HPP
#define PRIM_HPP

#include "Graph.hpp"
#include <vector>

// GENERALLY BETTER FOR DENSE GRAPHS

std::vector<Edge> primMST(const Graph& graph, std::string& time);

#endif // PRIM_HPP
