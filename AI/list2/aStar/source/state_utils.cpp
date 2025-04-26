#include "state_utils.hpp"
#include <iostream>
#include <stdexcept>

State convertToState(const std::vector<std::vector<int>>& state) 
{
    if(state.size() != 4 || state[0].size() != 4) 
    {
        throw std::invalid_argument("State must be a 4x4 matrix.");
    }

    State result = 0;
    for (int i = 0; i < 16; ++i) 
    {
        result |= ((State) state[3 - (i / 4)][3 - (i % 4)] << (i * 4));
    }
    return result;
}

std::vector<std::vector<int>> convertState(State state) 
{
    std::vector<std::vector<int>> result(4, std::vector<int>(4));
    for (int i = 0; i < 16; ++i) 
    {
        result[3 - (i / 4)][3 - (i % 4)] = (state >> (i * 4)) & 0xF;
    }
    return result;
}

void printStateHex(State state) 
{
    std::cout << "State: 0x" << std::hex << std::uppercase << state << std::endl;
}

void printState(const std::vector<std::vector<int>>& state) 
{
    for (const auto& row : state) 
    {
        for (const auto& val : row) 
        {
            std::cout << val << " ";
        }
        std::cout << '\n';
    }
}

bool isSolved(State state) 
{
    return state == GOAL;
}

// DOESNT WORK ?????
bool isSolvable(State state) 
{   
    int inversions = 0;
    std::vector<int> flatState(16);

    for (int i = 0; i < 16; ++i) 
    {
        flatState[i] = (state >> (i * 4)) & 0xF;
    }

    for (int i = 0; i < 15; ++i) 
    {
        for (int j = i + 1; j < 16; ++j) 
        {
            if (flatState[i] != 0 && flatState[j] != 0 && flatState[i] > flatState[j]) 
            {
                ++inversions;
            }
        }
    }

    return inversions % 2 == 0;
}
