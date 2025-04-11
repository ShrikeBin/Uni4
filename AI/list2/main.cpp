#define State uint64_t
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>

// state looks like this:
// 0000 0000 0000 0000
// 0000 0000 0000 0000
// 0000 0000 0000 0000
// 0000 0000 0000 0000

// A B C D
// E F G H
// I J K L
// M N O P

// A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

// 64 bits 
// 16 * 4 bits
// 0 - 15
State convertToState (std::vector<std::vector<int>> state) 
{
    if(state.size() != 4 || state[0].size() != 4) 
    {
        throw std::invalid_argument("State must be a 4x4 matrix.");
    }

    State result = 0x0000000000000000;

    for (int i = 0; i < 16; ++i) 
    {
        // convert from 2D array to uint64
        // yes its stupid...
        result |= ((State) state[3 - (i / 4)][3- (i % 4)] << (i * 4));
    }
    return result;
}

std::vector<std::vector<int>> convertState(State state) 
{
    std::vector<std::vector<int>> result(4, std::vector<int>(4));

    for (int i = 0; i < 16; ++i) 
    {
        // convert from uint64 to 2D array
        // yes its stupid
        result[3 - (i / 4)][3- (i % 4)] = (state >> (i * 4)) & 0xF;
    }
    return result;
}

void printStateHex(State state) 
{
    std::cout << "State: 0x" << std::hex << std::uppercase << state << std::endl;
}

void printState(std::vector<std::vector<int>> state)
{
    for (const auto& row : state) 
    {
        for (const auto& val : row) 
        {
            std::cout << val << " ";
        }
        std::cout << std::endl;
    }
} 

bool isSolved(State state) 
{
    if(state == 0x123456789ABCDEF0) 
    {
        return true;
    }
    return false;
}

int main() 
{
    // test Translation
    std::vector<std::vector<int>> state = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 0}
    };

    State s = convertToState(state);
    printStateHex(s);
    std::cout << "Is solved: " << (isSolved(s) ? "Yes" : "No") << std::endl;

    auto convertedBack = convertState(s);
    std::cout << "Converted back:" << std::endl;
    printState(convertedBack);

    return 0;
}