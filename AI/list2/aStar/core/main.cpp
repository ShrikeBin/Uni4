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
#include <random>

/*
ps aux | grep <program_name> | grep -v grep | awk '{printf "%s %.2f GB\n", $11, $6/1024/1024}'
*/
int main() 
{
    using Clock = std::chrono::high_resolution_clock;
    DisjointPatternDB::load1("../precompute/disjoint_pattern_1.bin");
    DisjointPatternDB::load2("../precompute/disjoint_pattern_2.bin");
    DisjointPatternDB::load3("../precompute/disjoint_pattern_3.bin");

    std::array<uint8_t, 16> shuffled = {
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12,
        13, 14, 15, 0
    };
    std::random_device rd;
    std::mt19937 g(rd());

    while(true) 
    {
        std::shuffle(shuffled.begin(), shuffled.end(), g);
        if(isSolvable(convertToState(shuffled)))
        {
            break;
        }
    }
    
    State start = convertToState(shuffled);
    std::cout << "Initial state is solvable.\n";
    std::cout << "Initial state heuristic: " << (int) heuristics(start) << "\n";
    printStateHex(start);
    printState(shuffled);
    std::cout << "Beginning search...\n";

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
        std::cout << "Goal found!\n";
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
        outfile << "--------------------\n";
        // Optionally print states
        for (size_t i = 0; i < path.size(); ++i) 
        {
            outfile << "Step " << i << ":\n";
            const auto state = convertState(path[i]);

            for (int row = 0; row < 4; ++row) 
            {
                for (int col = 0; col < 4; ++col) 
                {
                    if(state[row * 4 + col] == 0) 
                    {
                        outfile << ". ";
                    } 
                    else 
                    {
                        outfile << static_cast<int> (state[row * 4 + col]) << " ";
                    }
                }
                outfile << '\n';
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


