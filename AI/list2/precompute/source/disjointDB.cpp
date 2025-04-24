#include "disjointDB.hpp"
#include <iostream>
#include <iomanip>
#include <bitset>

const int EXPECTED_SIZE = 16*15*14*13*12; // Expected size of the pattern database

void DisjointPatternDB::build() 
{
    pattern_db_1.reserve(EXPECTED_SIZE);
    build_pattern_1();
    //pattern_db_2.reserve(EXPECTED_SIZE);
    //build_pattern_2();
    //pattern_db_3.reserve(EXPECTED_SIZE);
    //build_pattern_3();
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
    std::map<std::array<uint8_t, 16>, uint8_t> cost_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode1(goal, 0);
    cost_map[goal] = 0;

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
            std::swap(new_state[zero_pos], new_state[npos]);
            uint8_t new_cost = dist + 1;

            // to zero tutaj coś psuje mocno ale już nie mam siły naprawić
            // TODO tu jest błąd, z dodawaniem do mapy
            // bo się powtarzają stany
            // sprawdzić czy już jest w mapie i do save'a dodać bez 0 dlatego się wali

            // Dobra już wiem co -> może dotrzeć do danego state na różne sposoby więc rózni się zerami, potrzeuję kolejnej mapy
            // w której będę trzymał stany i ich koszt ale bez zer
            // i tam mogę updateować bez zer o powtórki i będzie ok
            if (!cost_map.count(new_state)) 
            {
                cost_map.insert(std::make_pair(new_state, new_cost));
                q.push({new_state, new_cost});
            }
            else if(cost_map.count(new_state) && new_cost < cost_map[new_state])
            {
                cost_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
        //std::cout<<"Queue size: " << q.size() << "\n";
    }

    std::ofstream outfile("pattern_1.txt");

    for (const std::pair<const std::array<uint8_t, 16>, uint8_t>& entry : cost_map)
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
        0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 6, 7, 8,
        9, 10, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0
    };

    std::queue<std::pair<std::array<uint8_t, 16>, uint8_t>> q;
    std::map<std::array<uint8_t, 16>, uint8_t> cost_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode2(goal, 0);
    cost_map[goal] = 0;

    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    while (!q.empty()) 
    {
        /*auto q_copy = q; // copy the queue

        while (!q_copy.empty()) {
            const auto& [state, cost] = q_copy.front();

            std::cout << "Cost: " << (int)cost << "\nState:\n";
            for (int i = 0; i < 16; ++i) {
                if (i % 4 == 0) std::cout << "\n";
                if (state[i] == 0xFF) std::cout << " . ";
                else std::cout << std::setw(2) << std::setfill(' ') << (int)state[i] << " ";
            }
            std::cout << "\n\n";

            q_copy.pop();
        }*/

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
            std::swap(new_state[zero_pos], new_state[npos]);
            /*std::cout << "New State:\n";
            for (int i = 0; i < 16; ++i) {
                if (i % 4 == 0) std::cout << "\n";  // New line every 4 elements for grid format
                if (new_state[i] == 0xFF) 
                    std::cout << " . ";  // Representing 0xFF as a dot
                else
                    std::cout << std::setw(2) << std::setfill(' ') << (int)new_state[i] << " ";
            }
            std::cout << "\n\n";*/

            uint8_t new_cost = dist + 1;

            if (!cost_map.count(new_state) || new_cost < cost_map[new_state]) 
            {
                cost_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
        std::cout<<"Queue size: " << q.size() << "\n";
    }

    for (const auto& [code, cost] : cost_map)
    {
        pattern_db_2.emplace_back(encode2(code, cost));
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
    std::map<std::array<uint8_t, 16>, uint8_t> cost_map; // map encoded state (without cost) -> min cost

    q.push({goal, 0});
    uint32_t start_code = encode3(goal, 0);
    cost_map[goal] = 0;

    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    while (!q.empty()) 
    {
        /*auto q_copy = q; // copy the queue

        while (!q_copy.empty()) {
            const auto& [state, cost] = q_copy.front();

            std::cout << "Cost: " << (int)cost << "\nState:\n";
            for (int i = 0; i < 16; ++i) {
                if (i % 4 == 0) std::cout << "\n";
                if (state[i] == 0xFF) std::cout << " . ";
                else std::cout << std::setw(2) << std::setfill(' ') << (int)state[i] << " ";
            }
            std::cout << "\n\n";

            q_copy.pop();
        }*/

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
            std::swap(new_state[zero_pos], new_state[npos]);
            /*std::cout << "New State:\n";
            for (int i = 0; i < 16; ++i) {
                if (i % 4 == 0) std::cout << "\n";  // New line every 4 elements for grid format
                if (new_state[i] == 0xFF) 
                    std::cout << " . ";  // Representing 0xFF as a dot
                else
                    std::cout << std::setw(2) << std::setfill(' ') << (int)new_state[i] << " ";
            }
            std::cout << "\n\n";*/

            uint8_t new_cost = dist + 1;

            if (!cost_map.count(new_state) || new_cost < cost_map[new_state]) 
            {
                cost_map[new_state] = new_cost;
                q.push({new_state, new_cost});
            }
        }
        std::cout<<"Queue size: " << q.size() << "\n";
    }

    for (const auto& [code, cost] : cost_map)
    {
        pattern_db_3.emplace_back(encode3(code, cost));
    }
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
