#ifndef STATE_UTILS_HPP
#define STATE_UTILS_HPP

#include <cstdint>
#include <array>
#include <fstream>
#include <sstream>

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

const uint64_t GOAL = 0x123456789ABCDEF0;
using State = uint64_t;

State convertToState(const std::array<uint8_t, 16>& state);
std::array<uint8_t, 16> convertState(State state);

void printStateHex(State state);
void printState(const std::array<uint8_t, 16>& state);

bool isSolved(State state);
bool isSolvable(State state);

std::array<uint8_t, 16> readBoardCSV(const std::string& filename);

#endif // STATE_UTILS_HPP