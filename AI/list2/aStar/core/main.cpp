#include "state_utils.hpp"
#include "search.hpp"
#include "heuristics.hpp"
#include <iostream>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <queue>
#include <chrono>
#include <algorithm>

/*
ps aux | grep <program_name> | grep -v grep | awk '{printf "%s %.2f GB\n", $11, $6/1024/1024}'
*/
int main() 
{
    using Clock = std::chrono::high_resolution_clock;
    DisjointPatternDB::load1("../precompute/disjoint_pattern_1.bin");
    DisjointPatternDB::load2("../precompute/disjoint_pattern_2.bin");
    DisjointPatternDB::load3("../precompute/disjoint_pattern_3.bin");

    std::vector<std::vector<int>> initial_state1 = {
        {0, 15, 14, 13},
        {12, 11, 10, 9},
        {8,7,6,5},
        {4,3,2,1}
    };

    std::vector<std::vector<int>> initial_state2 = {
        {15,14,8,12},
        {10,11,9,13},
        {2,6,5,1},
        {3,7,4,0}
    };

    State start = convertToState(initial_state2);
    if(!isSolvable(start)) 
    {
        std::cout << "Initial state is not solvable.\n";
        return 0;
    }
    std::cout << "Initial state is solvable.\n";
    std::cout << "Initial state heuristic: " << (int) heuristics(start) << "\n";

    std::priority_queue<Node, std::vector<Node>, std::greater<Node>> open;
    std::unordered_set<State> visited;
    std::unordered_map<State, State> came_from;

    open.push(Node{start, 0, heuristics(start)});
    visited.insert(start);

    auto start_time = Clock::now();

    bool found = false;
    Node goal_node = Node{0, 0, 0};

    while (!found || !open.empty()) 
    {
        Node current = open.top();
        open.pop();

        if (current.state == GOAL) 
        {
            goal_node = current;
            found = true;
            break;
        }

        NeighborList neighbors = getNeighbors(current.state, current.g);
        for (const Node& neighbor : neighbors) 
        {
            if (neighbor.state != 0 && visited.find(neighbor.state) == visited.end()) 
            {
                open.push(neighbor);
                visited.insert(neighbor.state);
                came_from[neighbor.state] = current.state;
            }
        }
    }

    auto end_time = Clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    if (found) 
    {
        std::ofstream outfile("solution.txt");
        std::vector<State> path;
        State current = goal_node.state;
        while (current != start) 
        {
            path.push_back(current);
            current = came_from[current];
        }
        path.push_back(start);

        std::reverse(path.begin(), path.end());

        outfile << "Goal found!\n";
        outfile << "Steps: " << (path.size() - 1) << "\n";
        outfile << "Time taken: " << duration << " ms\n";
        outfile << "Initial state heuristic: " << (int) heuristics(start) << "\n";
        outfile << "Nodes visited: " << visited.size() << "\n";
        outfile << "Path:\n";

        // Optionally print states
        for (size_t i = 0; i < path.size(); ++i) 
        {
            outfile << "Step " << i << ":\n";
            const auto state = convertState(path[i]);
            for (const auto& row : state) 
            {
                for (const auto& val : row) 
                {
                    if(val == 0) 
                    {
                        outfile << ". ";
                    } 
                    else 
                    {
                        outfile << val << " ";
                    }
                }
                outfile << "\n";
            }
            outfile << "--------------------\n";
        }
    } 
    else 
    {
        std::cout << "No solution found.\n";
    }

    return 0;
}


