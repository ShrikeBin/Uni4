#include "search.hpp"
#include <unordered_map>

// very temporary
uint16_t heuristics(State state)
{
    return 0;
}

NeighborList getNeighbors(State state, uint16_t g)
{
    uint8_t zero_pos = 0;
    // TODO FIX IT
    // that's a wrong way of checking equality....
    while(state >> (zero_pos*4) == 0x0 && zero_pos<= 15)
    {
        ++zero_pos;
    }
    // found 0 position
    uint8_t move_mask = valid_moves[zero_pos];
    NeighborList neighbors = {};
    
    // just so you know:
    // ^= AND
    // |= OR

    //  lets say i have state = 0xAAAAAAAAAAAAAAA0
    //  if left:
    //  0xAAAAAAAAAAAAAAAA (set where 0 is to what left of 0 was)
    //  and then
    //  0xAAAAAAAAAAAAAA0A (set the left of 0 was to 0)

    if (move_mask & 0b1000) // right is valid (1st bit)
    {
    State new_state = state;
    
    // Step 1: Remove the 4 bits at `zero_pos`
    new_state ^= (0xF << (zero_pos * 4)); // Clear the 4 bits at `zero_pos`
    
    // Step 2: Set the 4 bits at `zero_pos + 1` to the value of the current position
    new_state |= (0xF << ((zero_pos + 1) * 4)); // Set the 4 bits at `zero_pos + 1`
    
    // Step 3: Set the 4 bits at `zero_pos` to 0 (since it's now moved)
    new_state ^= (0xF << ((zero_pos + 1) * 4)); // Clear the 4 bits at `zero_pos + 1` to avoid duplicates
    
    neighbors[0] = Node{new_state, g + 1, heuristics(new_state)}; 
    }

    if (move_mask & 0b0100) // left is valid (2nd bit)
    {
        State new_state = state;
        new_state ^= (0xF << (zero_pos * 4));
        new_state |= (0xF << ((zero_pos - 1) * 4));

        neighbors[1] = Node{new_state, g + 1, heuristics(new_state)}; 
    }

    if (move_mask & 0b0010) // up is valid (3rd bit)
    {
        State new_state = state;
        new_state ^= (0xF << (zero_pos * 4));
        new_state |= (0xF << ((zero_pos - 4) * 4));

        neighbors[2] = Node{new_state, g + 1, heuristics(new_state)}; 
    }

    if (move_mask & 0b0001) // down is valid (4th bit)
    {
        State new_state = state;
        new_state ^= (0xF << (zero_pos * 4));
        new_state |= (0xF << ((zero_pos + 4) * 4));

        neighbors[3] = Node{new_state, g + 1, heuristics(new_state)}; 
    }
}
