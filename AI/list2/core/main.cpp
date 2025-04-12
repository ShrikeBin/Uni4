#include "state_utils.hpp"
#include <iostream>

int main() 
{
    std::vector<std::vector<int>> state = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 0}
    };

    State s = convertToState(state);
    printStateHex(s);
    std::cout << "Is solved: " << (isSolved(s) ? "Yes" : "No") << std::endl;
    std::cout << "Is solvable: " << (isSolvable(s) ? "Yes" : "No") << std::endl;

    auto convertedBack = convertState(s);
    std::cout << "Converted back:\n";
    printState(convertedBack);

    return 0;
}
