#ifndef SEARCH_HPP
#define SEARCH_HPP

#include <array>
#include <vector>
#include "state_utils.hpp"

struct Node 
{
    State state;
    uint16_t g;
    uint16_t h;
    uint16_t f; 
    
    Node(State s, uint16_t G, uint16_t H)
    :state(s), g(G), h(H), f(G+H) {}

    Node()
    :state(0), g(0), h(0), f(0) {}

    bool operator > (const Node& other) const 
    {
        return f > other.f;
    }
    bool operator < (const Node& other) const 
    {
        return f > other.f;
    }
};

// each position valid moves representation
// 16 values (0..15) going the same as the state
// 0b(right)(left)(up)(down)
static constexpr std::array<uint8_t, 16> valid_moves = 
{{
    0b1001,  // Position [0,0] valid: right, down
    0b1101,  // Position [1,0] valid: right, left, down
    0b1110,  // Position [2,0] valid: right, left, down
    0b0111,  // Position [3,0] valid: left, down
    0b1011,  // Position [0,1] valid: right, down, left
    0b1110,  // Position [1,1] valid: right, down, left
    0b1110,  // Position [2,1] valid: right, down, left
    0b0111,  // Position [3,1] valid: left, down
    0b1101,  // Position [0,2] valid: right, left, up
    0b1110,  // Position [1,2] valid: right, left, up
    0b1110,  // Position [2,2] valid: right, left, up
    0b0111,  // Position [3,2] valid: left, up
    0b1011,  // Position [0,3] valid: right, up, left
    0b1101,  // Position [1,3] valid: right, left, up
    0b1110,  // Position [2,3] valid: right, left, up
    0b0110   // Position [3,3] valid: left, up
}};

typedef std::array<Node, 4> NeighborList;
NeighborList getNeighbors(State state);

#endif // SEARCH_HPP