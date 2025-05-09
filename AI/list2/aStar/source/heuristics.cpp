#include "heuristics.hpp"

// HERE YOU CAN CHANGE THE HEURISTIC FUNCTION
// YES ITS HARD CODED
// BUT THIS WAY ITS FASTER
uint8_t heuristics(State state) 
{
    uint8_t mdid = MDID(state);
    //uint8_t disjoint = DisjointPatternDB::get_heuristic(state);

    //return mdid > disjoint ? mdid : disjoint;
    return mdid;
    //return disjoint;
}

std::array<uint8_t, 1 << 20> DisjointPatternDB::pattern_db_1{};
std::array<uint8_t, 1 << 20> DisjointPatternDB::pattern_db_2{};
std::array<uint8_t, 1 << 20> DisjointPatternDB::pattern_db_3{};

void DisjointPatternDB::load1(const std::string& filename) 
{
    std::ifstream file(filename, std::ios::binary);
    uint32_t data;
    while (file.read(reinterpret_cast<char*>(&data), sizeof(data))) 
    {
        uint32_t key = data >> 12;
        uint8_t value = data & 0xFF;     
        pattern_db_1[key] = value;
    }
}

void DisjointPatternDB::load2(const std::string& filename) 
{
    std::ifstream file(filename, std::ios::binary);
    uint32_t data;
    while (file.read(reinterpret_cast<char*>(&data), sizeof(data))) 
    {
        uint32_t key = data >> 12;
        uint8_t value = data & 0xFF;
        pattern_db_2[key] = value;
    }
}

void DisjointPatternDB::load3(const std::string& filename) 
{
    std::ifstream file(filename, std::ios::binary);
    uint32_t data;
    while (file.read(reinterpret_cast<char*>(&data), sizeof(data))) 
    {
        uint32_t key = data >> 12;
        uint8_t value = data & 0xFF;
        pattern_db_3[key] = value;
    }
}


uint8_t DisjointPatternDB::get_heuristic(State state)
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

    uint8_t total = pattern_db_1[key1] + pattern_db_2[key2] + pattern_db_3[key3];
    return total;
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
