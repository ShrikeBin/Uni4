#ifndef STATE_UTILS_HPP
#define STATE_UTILS_HPP

#include <vector>
#include <cstdint>

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

State convertToState(const std::vector<std::vector<int>>& state);
std::vector<std::vector<int>> convertState(State state);

void printStateHex(State state);
void printState(const std::vector<std::vector<int>>& state);

bool isSolved(State state);
bool isSolvable(State state);

#endif // STATE_UTILS_HPP