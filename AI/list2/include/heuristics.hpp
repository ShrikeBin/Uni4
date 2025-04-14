#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include "state_utils.hpp"

uint16_t heuristics(State state);

uint16_t jointPatternDatabase(State state);
uint16_t walkingDistance(State state);

#endif // HEURISTICS_HPP