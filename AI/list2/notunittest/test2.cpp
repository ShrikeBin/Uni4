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
    // 1,5,6,9,10
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if(state[i] == 1)
        {
            pattern_bits |= (i << 16);
        }
        else if(state[i] == 5)
        {
            pattern_bits |= (i << 12);
        }
        else if(state[i] == 6)
        {
            pattern_bits |= (i << 8);
        }
        else if(state[i] == 9)
        {
            pattern_bits |= (i << 4);
        }
        else if(state[i] == 10)
        {
            pattern_bits |= i;
        }
    }
    pattern_bits <<= 12; // for 4-bit pad + 8-bit heuristic
    pattern_bits |= heuristic;
    return pattern_bits;
}

uint32_t encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic)
{
    //2,3,4,7,8
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if(state[i] == 2)
        {
            pattern_bits |= (i << 16);
        }
        else if(state[i] == 3)
        {
            pattern_bits |= (i << 12);
        }
        else if(state[i] == 4)
        {
            pattern_bits |= (i << 8);
        }
        else if(state[i] == 7)
        {
            pattern_bits |= (i << 4);
        }
        else if(state[i] == 8)
        {
            pattern_bits |= i;
        }
    }
    pattern_bits <<= 12;
    pattern_bits |= heuristic;
    return pattern_bits;
}

uint32_t encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic)
{
    uint32_t pattern_bits = 0;
    for(uint8_t i = 0; i < 16; ++i) 
    {
        if(state[i] == 11)
        {
            pattern_bits |= (i << 16);
        }
        else if(state[i] == 12)
        {
            pattern_bits |= (i << 12);
        }
        else if(state[i] == 13)
        {
            pattern_bits |= (i << 8);
        }
        else if(state[i] == 14)
        {
            pattern_bits |= (i << 4);
        }
        else if(state[i] == 15)
        {   
            pattern_bits |= i;
        }
    }
    pattern_bits <<= 12;
    pattern_bits |= heuristic;
    return pattern_bits;
}

static inline uint8_t get_tile(uint64_t state, int idx) 
{
    return (state >> ((15 - idx) * 4)) & 0xF;
}

uint64_t getPos(uint64_t state)
{
    uint64_t positions = 0x0000000000000000;
    for(uint8_t i = 0; i < 16; i++)
    {
        uint8_t tile = get_tile(state, i);
        std::cout << (int) i << "th Tile is: " << (int)tile << "\n";
        if (tile == 0) continue;
        std::cout << "swap left by: " << (60 - (tile * 4)) << "\n";
        std::cout << "mask: "<< std::hex << ((uint64_t)i << (60 - (tile * 4))) << "\n";
        positions |=(((uint64_t)i) << (60 - (tile * 4)));
        std::cout << "current positions: " << positions << "\n" << std::dec;
    }
    return positions;
}

uint8_t get_heuristic(uint64_t state)
{
    uint64_t positions = 0x0000000000000000;
    for(uint8_t i = 0; i < 16; i++)
    {
        uint8_t tile = get_tile(state, i);
        if (tile == 0) continue;
        positions |=(((uint64_t)i) << (60 - (tile * 4)));
    }

    uint32_t key1 = 0x00000000;
    uint32_t key2 = 0x00000000;
    uint32_t key3 = 0x00000000;

    key1 |= (((uint32_t)get_tile(positions, 1)) << 16);
    key1 |= (((uint32_t)get_tile(positions, 5)) << 12);
    key1 |= (((uint32_t)get_tile(positions, 6)) << 8);
    key1 |= (((uint32_t)get_tile(positions, 9)) << 4);
    key1 |= ((uint32_t)get_tile(positions, 10));

    key2 |= (((uint32_t)get_tile(positions, 2)) << 16);
    key2 |= (((uint32_t)get_tile(positions, 3)) << 12);
    key2 |= (((uint32_t)get_tile(positions, 4)) << 8);
    key2 |= (((uint32_t)get_tile(positions, 7)) << 4);
    key2 |= ((uint32_t)get_tile(positions, 8));

    key3 |= (((uint32_t)get_tile(positions, 11)) << 16);
    key3 |= (((uint32_t)get_tile(positions, 12)) << 12);
    key3 |= (((uint32_t)get_tile(positions, 13)) << 8);
    key3 |= (((uint32_t)get_tile(positions, 14)) << 4);
    key3 |= ((uint32_t)get_tile(positions, 15));

    std::cout << "key1: " << std::hex << key1 << "\n";
    std::cout << "key2: " << std::hex << key2 << "\n";
    std::cout << "key3: " << std::hex << key3 << "\n" << std::dec;
    return 0;
}

int main() 
{
    std::array<uint8_t, 16> state1 = {
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 1, 5, 6,
        9, 10, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };
    std::array<uint8_t, 16> state2 = {
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 2, 3,
        4, 7, 8, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };
    std::array<uint8_t, 16> state3 = {
        11, 12, 13, 14,
        15, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };
    /*
    uint8_t heuristic = 5;
    uint32_t encoded1 = encode1(state1, heuristic);
    uint32_t encoded2 = encode2(state2, heuristic);
    uint32_t encoded3 = encode3(state3, heuristic);
    std::cout << "Encoded1 value: 0x" << std::hex << encoded1 << std::dec << "\n";
    std::cout << "Encoded2 value: 0x" << std::hex << encoded2 << std::dec << "\n";
    std::cout << "Encoded3 value: 0x" << std::hex << encoded3 << std::dec << "\n";
    */
    uint64_t stateA = 0x43218765CBA90FED;
    uint64_t stateB = 0x123456789ABCDEF0;
    get_heuristic(stateB);
    return 0;
}