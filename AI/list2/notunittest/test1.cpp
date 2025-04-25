#include <stdint.h>
#include <iostream>

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

uint8_t MDID(uint64_t state) 
{
    uint8_t totalMD = 0;
    uint8_t totalID = 0;

    // Manhattan distance
    for (int i = 0; i < 16; i++) 
    {
        uint8_t tile = get_tile(state, i);
        if (tile == 0) continue;
        uint8_t goal = tile - 1;
        int8_t dx = (i / 4) - (goal / 4);
        int8_t dy = (i % 4) - (goal % 4);
        totalMD += fast_abs_i8(dx) + fast_abs_i8(dy);
    }

    // Inversion distance
    uint8_t horizontal = inversionsH(state);
    uint8_t vertical = inversionsV(state);
    totalID = horizontal/3 + horizontal%3 + vertical/3 + vertical%3;

    return totalMD > totalID ? totalMD : totalID;
}

int main() 
{
    uint64_t state = 0x0FEDCBA987654321; // Example state
    uint8_t inversions_h = inversionsH(state);
    uint8_t inversions_v = inversionsV(state);

    std::cout << "Inversions H: " << static_cast<int>(inversions_h) << std::endl;
    std::cout << "Inversions V: " << static_cast<int>(inversions_v) << std::endl;
    std::cout << "MDID: " << static_cast<int>(MDID(state)) << std::endl;

    return 0;
}