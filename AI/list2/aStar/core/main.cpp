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
int main(int argc, char* argv[]) 
{
    if (!(argc == 2 || argc == 1)) 
    {
        std::cerr << "Usage: " << argv[0] << " <board.csv> <- read from file\n";
        std::cerr << "Usage: " << argv[0] << "  <- random board\n";
        return 1;
    }

    using Clock = std::chrono::high_resolution_clock;
    DisjointPatternDB::load1("../precompute/disjoint_pattern_1.bin");
    DisjointPatternDB::load2("../precompute/disjoint_pattern_2.bin");
    DisjointPatternDB::load3("../precompute/disjoint_pattern_3.bin");

    std::array<uint8_t, 16> board{};

    if(argc == 2) 
    {
        try 
        {
            board = readBoardCSV(argv[1]);
            std::cout << "Custom Board loaded\n";
            if(!isSolvable(convertToState(board))) 
            {
                std::cerr << "Board is not solvable.\n";
                return 1;
            }
            auto test = board;
            std::sort(test.begin(), test.end());
            for(uint8_t i = 0; i < 16; ++i) 
            {
                if(test[i] != i) 
                {
                    std::cerr << "Board is not valid.\n";
                    return 1;
                }
            }
        } 
        catch (const std::exception& e) 
        {
            std::cerr << "Error: " << e.what() << '\n';
            return 1;
        }
    }
    else
    {
        std::array<uint8_t, 16> board = {
            1, 2, 3, 4,
            5, 6, 7, 8,
            9, 10, 11, 12,
            13, 14, 15, 0
        };
        std::random_device rd;
        std::mt19937 g(rd());

        while(true) 
        {
            std::shuffle(board.begin(), board.end(), g);
            if(isSolvable(convertToState(board)))
            {
                break;
            }
        }
    }
    
    State start = convertToState(board);
    std::cout << "Initial state is solvable.\n";
    std::cout << "Initial state heuristic: " << (int) heuristics(start) << "\n";
    printStateHex(start);
    printState(board);
    std::cout << "Beginning search...\n";

    struct NodeCompare 
    {
        bool operator()(const Node& a, const Node& b) const 
        {
            if (a.f == b.f)
            {
                return a.h > b.h; // tie-break: prefer node closer to start (lower g)
            }
            else
            { 
                return a.f > b.f; // primary: prefer lower f
            }
        }
    };
    std::priority_queue<Node, std::vector<Node>, NodeCompare> open;
    std::unordered_map<State, State> came_from; // to reconstruct the path, i dont have a better idea
    std::unordered_map<State, uint8_t> g_score; // map to hold different nodes and their distances to reduce unnecessary exploring
    g_score[start] = 0;

    open.push(Node{start, 0, heuristics(start)});

    auto start_time = Clock::now();

    bool found = false;
    Node goal_node = Node{0, 0, 0};

    while (!open.empty()) 
    {
        Node current = open.top();
        open.pop();
        
        if (g_score.size() % 100'000 == 0) 
        {
            std::cout << "Differend nodes explored: " << g_score.size() << "\n";
        }

        if (current.state == GOAL) 
        {
            goal_node = current;
            found = true;
            break;
        }

        NeighborList neighbors = getNeighbors(current.state, current.g);
        for (const Node& neighbor : neighbors) 
        {
            if (neighbor.state == 0) continue;

            uint8_t unsure_g = current.g + 1;

            auto it = g_score.find(neighbor.state);
            // Check if the neighbor has been visited but cheaper
            if (it == g_score.end() || unsure_g < (it->second)) 
            {
                g_score[neighbor.state] = unsure_g;
                open.push(Node{neighbor.state, unsure_g, heuristics(neighbor.state)});
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
        outfile << "Nodes visited: " << g_score.size() << "\n";
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


