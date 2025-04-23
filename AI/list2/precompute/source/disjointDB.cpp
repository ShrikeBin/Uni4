#include "disjointDB.hpp"

void DisjointPatternDB::build() 
{
    build_pattern_1();
    build_pattern_2();
    build_pattern_3();
}

void DisjointPatternDB::save(const std::string& prefix) 
{
    // Save each pattern's database to a separate file
    std::ofstream out1(prefix + "pattern_1.bin", std::ios::binary);
    for (const uint32_t& entry : pattern_db_1) 
    {
        out1.write(reinterpret_cast<const char*>(&entry), sizeof(entry));
    }

    std::ofstream out2(prefix + "pattern_2.bin", std::ios::binary);
    for (const uint32_t& entry : pattern_db_2) 
    {
        out2.write(reinterpret_cast<const char*>(&entry), sizeof(entry));
    }

    std::ofstream out3(prefix + "pattern_3.bin", std::ios::binary);
    for (const uint32_t& entry : pattern_db_3) 
    {
        out3.write(reinterpret_cast<const char*>(&entry), sizeof(entry));
    }
}


void DisjointPatternDB::build_pattern_1() 
{
    std::array<uint8_t, 16> goal = {
        1, 2, 3, 4,
        5, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };

    std::queue<std::pair<std::array<uint8_t, 16>, uint8_t>> q;
    q.push({goal, 0});

    pattern_db_1.insert(encode1(goal, 0));

    // cholera wie co tu jest mam dość
    while (!q.empty()) 
    {
        std::array<uint8_t, 16> state = q.front().first;
        uint8_t dist = q.front().second;
        q.pop();

        int zero_pos = -1;
        for (int i = 0; i < 16; ++i) 
        {
            if (state[i] == 0) 
            {
                zero_pos = i;
                break;
            }
        }

        int x = zero_pos % 4;
        int y = zero_pos / 4;
        const int dx[4] = {-1, 1, 0, 0};
        const int dy[4] = {0, 0, -1, 1};

        for (int d = 0; d < 4; ++d) 
        {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || ny < 0 || nx >= 4 || ny >= 4) continue;

            int npos = ny * 4 + nx;

            // Only allow swapping with actual tiles (1-15 or 0xFF), block garbage
            if (state[npos] != 0 && state[npos] != 0xFF && state[npos] > 15) continue;

            std::array<uint8_t, 16> new_state = state;
            std::swap(new_state[zero_pos], new_state[npos]);

            uint32_t code = encode1(new_state, dist + 1);
            if (pattern_db_1.count(code) == 0) 
            {
                pattern_db_1.insert(code);
                q.push({new_state, dist + 1});
            }
        }
    }
}


void DisjointPatternDB::build_pattern_2() 
{
    
}

void DisjointPatternDB::build_pattern_3() 
{
    
}

uint32_t DisjointPatternDB::encode1(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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

uint32_t DisjointPatternDB::encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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

uint32_t DisjointPatternDB::encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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
