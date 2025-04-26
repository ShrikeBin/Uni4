#ifndef SEARCH_HPP
#define SEARCH_HPP

#include <array>
#include <vector>
#include "state_utils.hpp"

struct Node 
{
    State state;
    uint8_t g;
    uint8_t h;
    uint8_t f; 
    
    Node(State s, uint8_t G, uint8_t H)
    :state(s), g(G), h(H), f(G+H) {}
    ~Node() = default;

    bool operator > (const Node& other) const 
    {
        return f > other.f;
    }
    bool operator < (const Node& other) const 
    {
        return f > other.f;
    }
};

class BucketQueue 
{
    private:
        std::array<std::vector<Node>, 81> buckets;
        int current = 0;
    
    public:
        size_t size = 0;
        void push(const Node& node); 
        bool empty(); 
        Node pop();
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
