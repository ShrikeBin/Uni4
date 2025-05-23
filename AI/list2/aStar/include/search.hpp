#ifndef SEARCH_HPP
#define SEARCH_HPP

#include <array>
#include <vector>
#include "state_utils.hpp"
#include <cstdint>

struct Node 
{
    State state;
    uint32_t ghf;

    Node(State s, uint8_t G, uint8_t H)
    : state(s)
    {
        uint8_t F = G + H;
        ghf = (G << 16) | (H << 8) | F;
    }

    ~Node() = default;

    uint8_t g() const { return (ghf >> 16) & 0xFF; }
    uint8_t h() const { return (ghf >> 8) & 0xFF; }
    uint8_t f() const { return ghf & 0xFF; }

    bool operator > (const Node& other) const 
    {
        return f() > other.f();
    }
    
    bool operator < (const Node& other) const 
    {
        return f() < other.f();
    }
};


// each position valid moves representation
// 16 values (0..15) going the same as the state
// 0b(right)(left)(up)(down)
static constexpr std::array<uint8_t, 16> valid_moves = 
{{
    0b1001, // [0,0] → right, down
    0b1101, // [0,1] → right, left, down
    0b1101, // [0,2] → right, left, down
    0b0101, // [0,3] → left, down

    0b1011, // [1,0] → right, up, down
    0b1111, // [1,1] → all directions
    0b1111, // [1,2] → all directions
    0b0111, // [1,3] → left, up, down

    0b1011, // [2,0] → right, up, down
    0b1111, // [2,1] → all directions
    0b1111, // [2,2] → all directions
    0b0111, // [2,3] → left, up, down

    0b1010, // [3,0] → right, up
    0b1110, // [3,1] → right, left, up
    0b1110, // [3,2] → right, left, up
    0b0110  // [3,3] → left, up
}};


typedef std::array<Node, 4> NeighborList;
NeighborList getNeighbors(State state, uint8_t g);

#endif // SEARCH_HPP
