#include "search.hpp"
#include "heuristics.hpp"
#include "state_utils.hpp"
#include <unordered_map>
#include <iostream>
#include <bitset>

NeighborList getNeighbors(State state, uint8_t g)
{
    uint8_t zero_pos = 15;
    // TODO FIX IT
    while(((state >> ((zero_pos)*4)) & 0xF) != 0x0 && zero_pos >= 0)
    {
        --zero_pos;
    }
    // found 0 position
    uint8_t move_mask = valid_moves[15 - zero_pos];

    std::cout<< "zero at from left: " << 1 + (unsigned short) zero_pos << std::endl << "move mask is: " << std::bitset<4>(move_mask) << std::endl;

    NeighborList neighbors = {Node{0, 0, 0}, Node{0, 0, 0}, Node{0, 0, 0}, Node{0, 0, 0} };
    
    if (move_mask & 0b1000) // right is valid (1st bit)
    {
        std::cout<<"right"<<std::endl;

        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos - 1) * 4))) << 4);
        new_state &= (~(0xFULL << ((zero_pos - 1) * 4)));

        neighbors[0] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)}; 
    }

    if (move_mask & 0b0100) // left is valid (2nd bit)
    {
        std::cout<<"left"<<std::endl;

        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos + 1) * 4))) >> 4);
        new_state &= (~(0xFULL << ((zero_pos + 1) * 4)));

        neighbors[1] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    if (move_mask & 0b0010) // up is valid (3rd bit)
    {
        std::cout<<"up"<<std::endl;

        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos + 4) * 4))) >> 16);
        new_state &= (~(0xFULL << ((zero_pos + 4) * 4)));

        neighbors[2] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    if (move_mask & 0b0001) // down is valid (4th bit)
    {
        std::cout<<"down"<<std::endl;

        State new_state = state;
        new_state |= ((state & (0xFULL << ((zero_pos - 4) * 4))) << 16);
        new_state &= (~(0xFULL << ((zero_pos - 4) * 4)));

        neighbors[3] = Node{new_state, (uint8_t) (g + 1), heuristics(new_state)};  
    }

    return neighbors;
}
