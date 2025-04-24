#ifndef DISJOINTDB_HPP
#define DISJOINTDB_HPP

#include <vector>
#include <queue>
#include <array>
#include <cstdint>
#include <fstream>
#include <map>

class DisjointPatternDB 
{
public:
    void build();
    void save(const std::string& prefix);

private:
    // 32-bit encoded (20 pattern | 4 pad | 8 heuristic)
    // Temporary variables to store each pattern database
    std::vector<uint32_t> pattern_db_1;
    std::vector<uint32_t> pattern_db_2;
    std::vector<uint32_t> pattern_db_3;

    void build_pattern_1();
    void build_pattern_2();
    void build_pattern_3();
    uint32_t encode1(const std::array<uint8_t, 16>& state, uint8_t heuristic);
    uint32_t encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic);
    uint32_t encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic);
};

#endif // DISJOINTDB_HPP
