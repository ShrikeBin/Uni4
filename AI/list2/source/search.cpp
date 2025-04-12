#include <unordered_map>
#include <queue>
#include <vector>
#include <set>
#include "state_utils.hpp"

struct Node {
    State state;
    int g; // cost so far
    int f; // total = g + h

    bool operator>(const Node& other) const {
        return f > other.f;
    }
};

std::vector<State> getNeighbors(State state); // you implement this
int heuristic(State state); // declared elsewhere

State start = ...;
std::priority_queue<Node, std::vector<Node>, std::greater<>> open;
std::unordered_map<State, int> gScore;
std::unordered_map<State, State> cameFrom;

gScore[start] = 0;
open.push({start, 0, heuristic(start)});

while (!open.empty()) {
    Node current = open.top();
    open.pop();

    if (isSolved(current.state)) {
        // reconstruct path if you want
        break;
    }

    for (State neighbor : getNeighbors(current.state)) {
        int tentative_g = gScore[current.state] + 1; // every move = cost 1

        if (!gScore.count(neighbor) || tentative_g < gScore[neighbor]) {
            gScore[neighbor] = tentative_g;
            int f = tentative_g + heuristic(neighbor);
            open.push({neighbor, tentative_g, f});
            cameFrom[neighbor] = current.state;
        }
    }
}


struct FastHasher {
    size_t operator()(uint64_t x) const {
        x ^= x >> 33;
        x *= 0xff51afd7ed558ccdULL;
        x ^= x >> 33;
        x *= 0xc4ceb9fe1a85ec53ULL;
        x ^= x >> 33;
        return x;
    }
};

std::vector<State> getNeighbors(State state) {
    std::vector<State> neighbors;

    // Find index of 0 (blank)
    int zeroIndex = -1;
    for (int i = 0; i < 16; ++i) {
        if (((state >> (i * 4)) & 0xF) == 0) {
            zeroIndex = i;
            break;
        }
    }

    int row = zeroIndex / 4;
    int col = zeroIndex % 4;

    const int dir[4][2] = {
        {-1, 0}, // up
        {1, 0},  // down
        {0, -1}, // left
        {0, 1}   // right
    };

    for (auto& d : dir) {
        int newRow = row + d[0];
        int newCol = col + d[1];

        if (newRow >= 0 && newRow < 4 && newCol >= 0 && newCol < 4) {
            int newIndex = newRow * 4 + newCol;

            // Swap blank (0) with neighbor tile
            State tile = (state >> (newIndex * 4)) & 0xF;

            State nextState = state;

            // Set new blank
            nextState &= ~(0xFULL << (newIndex * 4)); // clear neighbor
            nextState |= (0ULL << (newIndex * 4));    // write 0 (blank)

            // Move tile into old blank
            nextState &= ~(0xFULL << (zeroIndex * 4)); // clear blank
            nextState |= (tile << (zeroIndex * 4));    // move tile in

            neighbors.push_back(nextState);
        }
    }

    return neighbors;
}
