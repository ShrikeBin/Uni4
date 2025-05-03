#include "state_utils.hpp"
#include <iostream>
#include <stdexcept>

State convertToState(const std::array<uint8_t, 16>& state) 
{
    State result = 0;
    for (int i = 0; i < 16; ++i) 
    {
        result |= static_cast<State>(state[15 - i] & 0xF) << (i * 4);
    }
    return result;
}

// Convert 64-bit State back to 4x4 matrix stored in flat array
std::array<uint8_t, 16> convertState(State state) 
{
    std::array<uint8_t, 16> result;
    for (int i = 0; i < 16; ++i) 
    {
        result[15 - i] = (state >> (i * 4)) & 0xF;
    }
    return result;
}

// Print State in hex format
void printStateHex(State state) 
{
    std::cout << "State: 0x" << std::hex << std::uppercase << state << std::dec << std::endl;
}

// Print flat 4x4 matrix in matrix layout
void printState(const std::array<uint8_t, 16>& state) 
{
    for (int row = 0; row < 4; ++row) 
    {
        for (int col = 0; col < 4; ++col) 
        {
            std::cout << static_cast<int>(state[row * 4 + col]) << ' ';
        }
        std::cout << '\n';
    }
}

bool isSolved(State state) 
{
    return state == GOAL;
}

// DOESNT WORK ????? (its weird somehow)
bool isSolvable(State state) 
{   
    int inversions = 0;
    int blankRow = 0;
    int blankCol = 0;
    std::array<uint8_t, 16> flatState = convertState(state);

    for (int i = 0; i < 15; ++i) 
    {
        for (int j = i + 1; j < 16; ++j) 
        {
            if (flatState[i] != 0 && flatState[j] != 0 && flatState[i] > flatState[j]) 
            {
                ++inversions;
            }
            else if (flatState[i] == 0) 
            {
                blankRow = i / 4;
                blankCol = i % 4;
            }
        }
    }

    int taxicab = (3 - blankRow) + (3 - blankCol);

    int result = inversions + taxicab;

    return (result % 2) == 0;
}
