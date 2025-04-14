#include "state_utils.hpp"
#include "search.hpp"
#include "heuristics.hpp"
#include <iostream>

int main() 
{
    std::vector<std::vector<int>> state = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 12, 15},
        {13, 14, 11, 0}
    };

    State s = convertToState(state);

    NeighborList neighbors = getNeighbors(s,0);
    std::cout << std::endl;

    for (int i = 0; i < 4; i++) 
    {
        if(neighbors[i] != nullptr)
        {
            std::cout << "Neighbor " << i << ": "<< std::endl;
            printStateHex(neighbors[i]->state);
            free(neighbors[i]);
        }
    }

    std::cout << std::endl;
    printStateHex(s);
    std::cout << "Is solved: " << (isSolved(s) ? "Yes" : "No") << std::endl;
    std::cout << "Is solvable: " << (isSolvable(s) ? "Yes" : "No") << std::endl;

    auto convertedBack = convertState(s);
    std::cout << "Converted back:\n";
    printState(convertedBack);

    return 0;
}
