#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include "state_utils.hpp"
#include <stdint.h>

uint8_t heuristics(State state);
uint8_t jointPatternDatabase(State state);
uint8_t mdlinear(State state);

static inline uint8_t get_tile(uint64_t state, int idx) 
{
    return (state >> ((15 - idx) * 4)) & 0xF;
}

static inline int fast_abs(int x) 
{
    return (x ^ (x >> 31)) - (x >> 31);
}

#endif // HEURISTICS_HPP