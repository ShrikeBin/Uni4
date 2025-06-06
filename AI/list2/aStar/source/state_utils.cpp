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
            }
        }
    }

    if(blankRow == 3 || blankRow == 1) 
    {
        return (inversions % 2) == 0;
    }
    else if(blankRow == 2 || blankRow == 0) 
    {
        return (inversions % 2) == 1;
    }
    else
    {
        return false; // Invalid blankRow
    }
}

std::array<uint8_t, 16> readBoardCSV(const std::string& filename) 
{
    std::ifstream file(filename);
    if (!file.is_open()) 
    {
        throw std::runtime_error("Could not open file: " + filename);
    }

    std::array<uint8_t, 16> board{};
    std::string line;
    int idx = 0;

    while (std::getline(file, line) && idx < 16) 
    {
        std::stringstream ss(line);
        std::string value;

        while (std::getline(ss, value, ';') && idx < 16) 
        {
            if (!value.empty())
            {
                board[idx++] = static_cast<uint8_t>(std::stoi(value));
            }
        }
    }

    if (idx != 16) 
    {
        throw std::runtime_error("Invalid input: expected 16 numbers.");
    }
    
    return board;
}
