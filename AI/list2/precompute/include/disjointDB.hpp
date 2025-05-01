#ifndef DISJOINTDB_HPP
#define DISJOINTDB_HPP

#include <vector>
#include <queue>
#include <array>
#include <cstdint>
#include <fstream>
#include <map>
#include <string>

struct BigPatternEntry {
    uint32_t value32;
    uint8_t value8;
};

class DisjointPatternDB 
{
public:
    void build555();
    void build78();
    void save555(const std::string& prefix);
    void save78(const std::string& prefix);

private:
    // 32-bit encoded (20 pattern | 4 pad | 8 heuristic)
    // Temporary variables to store each pattern database
    std::vector<uint32_t> pattern_db_1;
    std::vector<uint32_t> pattern_db_2;
    std::vector<uint32_t> pattern_db_3;

    std::vector<BigPatternEntry> pattern_db_7;
    std::vector<BigPatternEntry> pattern_db_8;

    void build_pattern_1();
    void build_pattern_2();
    void build_pattern_3();

    void build_pattern_7();
    void build_pattern_8();

    uint32_t encode1(const std::array<uint8_t, 16>& state, uint8_t heuristic);
    uint32_t encode2(const std::array<uint8_t, 16>& state, uint8_t heuristic);
    uint32_t encode3(const std::array<uint8_t, 16>& state, uint8_t heuristic);

    BigPatternEntry encode7(const std::array<uint8_t, 16>& state, uint8_t heuristic);
    BigPatternEntry encode8(const std::array<uint8_t, 16>& state, uint8_t heuristic);
};

#endif // DISJOINTDB_HPP
