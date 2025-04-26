#include <iostream>
#include <iomanip>
#include <cstdint>
#include <array>
#include <fstream>
#include <bitset>
#include <string>
#include <algorithm>

std::array<uint8_t, 1 << 20> pattern_db_1{};
std::array<uint8_t, 1 << 20> pattern_db_2{};
std::array<uint8_t, 1 << 20> pattern_db_3{};

void load1(const std::string& filename) 
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

void load2(const std::string& filename) 
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

void load3(const std::string& filename) 
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

static inline uint8_t get_tile(uint64_t state, int idx) 
{
    return (state >> ((15 - idx) * 4)) & 0xF;
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

    uint8_t total = pattern_db_1[key1] + pattern_db_2[key2] + pattern_db_3[key3];
    return total;
}


int main() 
{
    std::cout << "Loading pattern databases...\n";
    load1("../precompute/disjoint_pattern_1.bin");
    load2("../precompute/disjoint_pattern_2.bin");
    load3("../precompute/disjoint_pattern_3.bin");
    std::cout << "Loaded pattern databases.\n";

    uint64_t state = 0x0FEDCBA987654321;
    uint64_t state2 = 0x123056789ABCDEF4;
    std::cout << "heuristic: " << std::dec << (int)get_heuristic(state2) << "\n";
    return 0;
}
