#include <vector>
#include <queue>
#include <array>
#include <string>
#include <cstdint>
#include <algorithm>
#include <map>
#include <iostream>

uint32_t encode1(const std::array<uint8_t, 16>& state, uint8_t heuristic)
{
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if (state[i] != 0 && state[i] != 0xFF) 
        {
            // i chce zapisać state[i] to tam gdzie zapisuje tj state[4] niech będzie 1 to znaczy że na 1 chce zapisać 4
            pattern_bits |= (i << (20 - (state[i]*4)));
        }
    }
    pattern_bits <<= 12; // for 4-bit pad + 8-bit heuristic
    pattern_bits |= heuristic;
    return pattern_bits;
}

uint32_t encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic)
{
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if (state[i] != 0 && state[i] != 0xFF) 
        {
            // i chce zapisać state[i] to tam gdzie zapisuje tj state[4] niech będzie 1 to znaczy że na 1 chce zapisać 4
            pattern_bits |= (i << (20 - ((state[i]*4) - 5)));
        }
    }
    pattern_bits <<= 12; // for 4-bit pad + 8-bit heuristic
    pattern_bits |= heuristic;
    return pattern_bits;
}

uint32_t encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic)
{
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if (state[i] != 0 && state[i] != 0xFF) 
        {
            // i chce zapisać state[i] to tam gdzie zapisuje tj state[4] niech będzie 1 to znaczy że na 1 chce zapisać 4
            pattern_bits |= (i << (20 - ((state[i]*4) - 10)));
        }
    }
    pattern_bits <<= 12; // for 4-bit pad + 8-bit heuristic
    pattern_bits |= heuristic;
    return pattern_bits;
}

int main() 
{
    std::array<uint8_t, 16> state = {
        1, 2, 3, 4,
        5, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };
    uint8_t heuristic = 5;
    uint32_t encoded = encode1(state, heuristic);
    std::cout << "Encoded value: " << std::hex << encoded << std::dec << "\n";

    return 0;
}