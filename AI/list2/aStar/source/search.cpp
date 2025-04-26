#include "search.hpp"
#include "heuristics.hpp"
#include "state_utils.hpp"
#include <unordered_map>
#include <iostream>
#include <bitset>

void BucketQueue::push(const Node& node) 
{
    buckets[node.f].push_back(node);
    if (node.f < current) current = node.f;
    ++size;
}

bool BucketQueue::empty()
{
    return size == 0;
}

Node BucketQueue::pop()
{
    while (buckets[current].empty()) ++current;

    Node node = std::move(buckets[current].back());
    buckets[current].pop_back();
    --size;

    return node;
}

NeighborList getNeighbors(State state, uint8_t g)
{
    uint8_t zero_pos = 15;
    while(((state >> ((zero_pos)*4)) & 0xF) != 0x0 && zero_pos >= 0)
    {
        --zero_pos;
    }

    uint8_t move_mask = valid_moves[15 - zero_pos];

    NeighborList neighbors = {Node{0, 0, 0}, Node{0, 0, 0}, Node{0, 0, 0}, Node{0, 0, 0} };
    
    if (move_mask & 0b1000) // right is valid (1st bit)
    {
        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos - 1) * 4))) << 4);
        new_state &= (~(0xFULL << ((zero_pos - 1) * 4)));

        neighbors[0] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)}; 
    }

    if (move_mask & 0b0100) // left is valid (2nd bit)
    {
        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos + 1) * 4))) >> 4);
        new_state &= (~(0xFULL << ((zero_pos + 1) * 4)));

        neighbors[1] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    if (move_mask & 0b0010) // up is valid (3rd bit)
    {
        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos + 4) * 4))) >> 16);
        new_state &= (~(0xFULL << ((zero_pos + 4) * 4)));

        neighbors[2] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    if (move_mask & 0b0001) // down is valid (4th bit)
    {
        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos - 4) * 4))) << 16);
        new_state &= (~(0xFULL << ((zero_pos - 4) * 4)));

        neighbors[3] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    return neighbors;
}
