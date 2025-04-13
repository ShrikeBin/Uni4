#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include "state_utils.hpp"

int manhattanDistance(State state);
int misplacedTiles(State state);
int linearConflict(State state);
int mdlinearConflict(State state);
int walkingDistance(State state);

#endif // HEURISTICS_HPP