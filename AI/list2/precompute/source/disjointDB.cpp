#include "disjointDB.hpp"
#include <iostream>
#include <iomanip>
#include <bitset>
#include <string>

const int EXPECTED_SIZE = 16*15*14*13*12; // Expected size of the pattern database

void DisjointPatternDB::build() 
{
    pattern_db_1.reserve(EXPECTED_SIZE);
    build_pattern_1();
    pattern_db_2.reserve(EXPECTED_SIZE);
    build_pattern_2();
    pattern_db_3.reserve(EXPECTED_SIZE);
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
        1, 0xFF, 0xFF, 0xFF,
        5, 6, 0xFF, 0xFF,
        9, 10, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };

    std::queue<std::pair<std::array<uint8_t, 16>, uint8_t>> q;
    std::map<std::array<uint8_t, 16>, uint8_t> count_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode1(goal, 0);
    count_map[goal] = 0;

    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    while (!q.empty()) 
    {
        auto [state, dist] = q.front();
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

        for (int d = 0; d < 4; ++d) 
        {
            int nx = x + dx[d];
            int ny = y + dy[d];

            if (nx < 0 || ny < 0 || nx >= 4 || ny >= 4) 
            {
                continue;
            }

            int npos = ny * 4 + nx;

            if (state[npos] != 0 && state[npos] != 0xFF && state[npos] > 15)
            {
                continue;
            }

            auto new_state = state;

            // we touched something important
            uint8_t new_cost = dist;
            if(new_state[npos] != 0xFF)
            {
                ++new_cost;
            }

            std::swap(new_state[zero_pos], new_state[npos]);

            if (!count_map.count(new_state)) 
            {
                count_map.insert(std::make_pair(new_state, new_cost));
                q.push({new_state, new_cost});
            }
            else if(count_map.count(new_state) && new_cost < count_map[new_state])
            {
                count_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
    }

    // Remove duplicate entries and keep the minimum cost
    std::map<std::array<uint8_t, 16>, uint8_t> min_cost_map;
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry: count_map)
    {
        // Remove that annoying 0;
        std::array<uint8_t, 16> state = entry.first;
        for (uint8_t& val : state) 
        {
            if (val == 0) 
            {
                val = 0xFF;
            }
        }

        if(!min_cost_map.count(state))
        {
            min_cost_map.insert(std::make_pair(state, entry.second));
        }
        else if (entry.second < min_cost_map[state])
        {
            min_cost_map[state] = entry.second;
        }
    }

    std::ofstream outfile("pattern_1.txt");
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry : min_cost_map)
    {   
        
        for (int i = 0; i < 16; ++i){outfile << (int)entry.first[i] << " ";}
        outfile << "\nEncoded value: " << std::hex << std::bitset<32>(encode1(entry.first, entry.second)) << std::dec;
        outfile << "\nValue: " << (int)entry.second;
        outfile << "\nState:";
        for (int i = 0; i < 16; ++i) {
            if (i % 4 == 0) outfile << "\n";
            if (entry.first[i] == 0xFF || entry.first[i] == 0) outfile << " . ";
            else outfile << std::setw(2) << std::setfill(' ') << (int)entry.first[i] << " ";
        }
        outfile << "\n";
        
        pattern_db_1.emplace_back(encode1(entry.first, entry.second));
    }
}



void DisjointPatternDB::build_pattern_2() 
{
    std::array<uint8_t, 16> goal = {
        0xFF, 2, 3, 4,
        0xFF, 0xFF, 7, 8,
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };

    std::queue<std::pair<std::array<uint8_t, 16>, uint8_t>> q;
    std::map<std::array<uint8_t, 16>, uint8_t> count_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode2(goal, 0);
    count_map[goal] = 0;

    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    while (!q.empty()) 
    {
        auto [state, dist] = q.front();
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

        for (int d = 0; d < 4; ++d) 
        {
            int nx = x + dx[d];
            int ny = y + dy[d];

            if (nx < 0 || ny < 0 || nx >= 4 || ny >= 4) 
            {
                continue;
            }

            int npos = ny * 4 + nx;

            if (state[npos] != 0 && state[npos] != 0xFF && state[npos] > 15)
            {
                continue;
            }

            auto new_state = state;

            // we touched something important
            uint8_t new_cost = dist;
            if(new_state[npos] != 0xFF)
            {
                ++new_cost;
            }

            std::swap(new_state[zero_pos], new_state[npos]);

            if (!count_map.count(new_state)) 
            {
                count_map.insert(std::make_pair(new_state, new_cost));
                q.push({new_state, new_cost});
            }
            else if(count_map.count(new_state) && new_cost < count_map[new_state])
            {
                count_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
    }

    // Remove duplicate entries and keep the minimum cost
    std::map<std::array<uint8_t, 16>, uint8_t> min_cost_map;
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry: count_map)
    {
        // Remove that annoying 0;
        std::array<uint8_t, 16> state = entry.first;
        for (uint8_t& val : state) 
        {
            if (val == 0) 
            {
                val = 0xFF;
            }
        }

        if(!min_cost_map.count(state))
        {
            min_cost_map.insert(std::make_pair(state, entry.second));
        }
        else if (entry.second < min_cost_map[state])
        {
            min_cost_map[state] = entry.second;
        }
    }

    std::ofstream outfile("pattern_2.txt");
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry : min_cost_map)
    {   
        
        for (int i = 0; i < 16; ++i){outfile << (int)entry.first[i] << " ";}
        outfile << "\nEncoded value: " << std::hex << std::bitset<32>(encode2(entry.first, entry.second)) << std::dec;
        outfile << "\nValue: " << (int)entry.second;
        outfile << "\nState:";
        for (int i = 0; i < 16; ++i) {
            if (i % 4 == 0) outfile << "\n";
            if (entry.first[i] == 0xFF || entry.first[i] == 0) outfile << " . ";
            else outfile << std::setw(2) << std::setfill(' ') << (int)entry.first[i] << " ";
        }
        outfile << "\n";
        
        pattern_db_2.emplace_back(encode2(entry.first, entry.second));
    }
}

void DisjointPatternDB::build_pattern_3() 
{
    std::array<uint8_t, 16> goal = {
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 11, 12,
        13, 14, 15, 0
    };

    std::queue<std::pair<std::array<uint8_t, 16>, uint8_t>> q;
    std::map<std::array<uint8_t, 16>, uint8_t> count_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode3(goal, 0);
    count_map[goal] = 0;

    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    while (!q.empty()) 
    {
        auto [state, dist] = q.front();
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

        for (int d = 0; d < 4; ++d) 
        {
            int nx = x + dx[d];
            int ny = y + dy[d];

            if (nx < 0 || ny < 0 || nx >= 4 || ny >= 4) 
            {
                continue;
            }

            int npos = ny * 4 + nx;

            if (state[npos] != 0 && state[npos] != 0xFF && state[npos] > 15)
            {
                continue;
            }

            auto new_state = state;

            // we touched something important
            uint8_t new_cost = dist;
            if(new_state[npos] != 0xFF)
            {
                ++new_cost;
            }
            
            std::swap(new_state[zero_pos], new_state[npos]);

            if (!count_map.count(new_state)) 
            {
                count_map.insert(std::make_pair(new_state, new_cost));
                q.push({new_state, new_cost});
            }
            else if(count_map.count(new_state) && new_cost < count_map[new_state])
            {
                count_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
    }

    // Remove duplicate entries and keep the minimum cost
    std::map<std::array<uint8_t, 16>, uint8_t> min_cost_map;
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry: count_map)
    {
        // Remove that annoying 0;
        std::array<uint8_t, 16> state = entry.first;
        for (uint8_t& val : state) 
        {
            if (val == 0) 
            {
                val = 0xFF;
            }
        }

        if(!min_cost_map.count(state))
        {
            min_cost_map.insert(std::make_pair(state, entry.second));
        }
        else if (entry.second < min_cost_map[state])
        {
            min_cost_map[state] = entry.second;
        }
    }

    std::ofstream outfile("pattern_3.txt");
    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry : min_cost_map)
    {   
        
        for (int i = 0; i < 16; ++i){outfile << (int)entry.first[i] << " ";}
        outfile << "\nEncoded value: " << std::hex << std::bitset<32>(encode3(entry.first, entry.second)) << std::dec;
        outfile << "\nValue: " << (int)entry.second;
        outfile << "\nState:";
        for (int i = 0; i < 16; ++i) {
            if (i % 4 == 0) outfile << "\n";
            if (entry.first[i] == 0xFF || entry.first[i] == 0) outfile << " . ";
            else outfile << std::setw(2) << std::setfill(' ') << (int)entry.first[i] << " ";
        }
        outfile << "\n";
        
        pattern_db_3.emplace_back(encode3(entry.first, entry.second));
    }
}

uint32_t DisjointPatternDB::encode1(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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

uint32_t DisjointPatternDB::encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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

uint32_t DisjointPatternDB::encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic)
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
