#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include "state_utils.hpp"
#include <stdint.h>
#include <array>
#include <string>
#include <fstream>

uint8_t heuristics(State state);
uint8_t MDID(State state);

class DisjointPatternDB 
{
    public:
        static std::array<uint8_t, 1 << 20> pattern_db_1;
        static std::array<uint8_t, 1 << 20> pattern_db_2;
        static std::array<uint8_t, 1 << 20> pattern_db_3;
    
    public:
        static void load1(const std::string& filename);
        static void load2(const std::string& filename);
        static void load3(const std::string& filename);
    
        static uint8_t get_heuristic(State state);
};
    



// INLINE FUNCTIONS
// These functions are defined inline for performance reasons.
static inline uint8_t get_tile(uint64_t state, int idx) 
{
    return (state >> ((15 - idx) * 4)) & 0xF;
}

static inline int8_t fast_abs_i8(int8_t x) 
{
    return (x ^ (x >> 7)) - (x >> 7);
}

//const uint8_t VERTICAL_PATTERN[16]= {1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,0};   // 1.5.9.13 | 2.6.10.14 | 3.7.11.15 | 4.8.12.0
const uint8_t VERTICAL_POSITION[16]= {15,0,4,8,12,1,5,9,13,2,6,10,14,3,7,11};  // [i] reprezentuje na której pozycji jest i w goalu
//                                                                             // [0] = 15 bo na 15 miejscu jest 0
//                                                                             // [1] = 0 bo na 0 miejscu jest 1
//                                                                             // [2] = 4 bo na 4 miejscu jest 2
//                                                                             // [3] = 8 bo na 8 miejscu jest 3

// XD
static inline uint64_t makeVertical(const uint64_t perm) 
{
    uint64_t vperm = 0x0000000000000000;

    vperm |= (uint64_t)(get_tile(perm, 0) << 4) << 56;
    vperm |= (uint64_t)(get_tile(perm, 1)) << 44;
    vperm |= (uint64_t)(get_tile(perm, 2)) << 28;
    vperm |= (uint64_t)(get_tile(perm, 3)) << 12;
    
    vperm |= (uint64_t)(get_tile(perm, 4) << 4) << 56;
    vperm |= (uint64_t)(get_tile(perm, 5)) << 40;
    vperm |= (uint64_t)(get_tile(perm, 6)) << 24;
    vperm |= (uint64_t)(get_tile(perm, 7)) << 8;
    
    vperm |= (uint64_t)(get_tile(perm, 8) << 4) << 52;
    vperm |= (uint64_t)(get_tile(perm, 9)) << 36;
    vperm |= (uint64_t)(get_tile(perm,10)) << 20;
    vperm |= (uint64_t)(get_tile(perm,11)) << 4;
    
    vperm |= (uint64_t)(get_tile(perm,12) << 4) << 48;
    vperm |= (uint64_t)(get_tile(perm,13)) << 32;
    vperm |= (uint64_t)(get_tile(perm,14)) << 16;
    vperm |= (uint64_t)(get_tile(perm,15));

    return vperm;
}

static inline uint8_t inversionsV(const uint64_t perm) 
{
    uint8_t inversions = 0;
    const uint64_t vperm = makeVertical(perm);
    for (int i = 0; i < 16; i++) 
    {
        uint8_t tile1 = get_tile(vperm, i);
        if (tile1 == 0) continue; // Skip blank tile

        for (int j = i + 1; j < 16; j++) 
        {
            uint8_t tile2 = get_tile(vperm, j);
            if (tile2 == 0) continue; // Skip blank tile

            // Compare positions in VERTICAL_PATTERN
            if (VERTICAL_POSITION[tile1] > VERTICAL_POSITION[tile2]) 
            {
                ++inversions;
            }
        }
    }
    return inversions;
}

// easier to do - intuitive inversions
static inline uint8_t inversionsH(const uint64_t perm) 
{
    uint8_t inversions = 0;

    for (int i = 0; i < 16; i++) 
    {
        uint8_t tile1 = get_tile(perm, i);
        if (tile1 == 0) continue; 

        for (int j = i + 1; j < 16; j++) 
        {
            uint8_t tile2 = get_tile(perm, j);
            if(tile2 == 0) continue;
            if(tile1 > tile2)
            {
                ++inversions;
            }
        }
    }

    return inversions;
}

#endif // HEURISTICS_HPP