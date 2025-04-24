#include "heuristics.hpp"

uint8_t jointPatternDatabase(State state)
{
    // tutaj będzie wczytywanie z pliku do idk np 3 tabel?
    // już nie pamiętam za dużo dzisiaj Walking Distance
    return 0;
}


// TODO make it max{MD, Inversion Distance} - cheaper!!
// to be fixed when you'll be aware of what you are doing...
uint8_t mdlinear(uint64_t state) 
{
    int total = 0;

    // Manhattan distance
    for (int i = 0; i < 16; i++) {
        uint8_t t = get_tile(state, i);
        if (t == 0) continue;
        int goal = t - 1;
        int dx = (i / 4) - (goal / 4);
        int dy = (i % 4) - (goal % 4);
        total += fast_abs(dx) + fast_abs(dy);
    }

    // Linear conflict - rows
    for (int row = 0; row < 4; row++) {
        for (int i = 0; i < 4; i++) {
            int idx_i = row * 4 + i;
            uint8_t t_i = get_tile(state, idx_i);
            if (t_i == 0 || (t_i - 1) / 4 != row) continue;

            for (int j = i + 1; j < 4; j++) {
                int idx_j = row * 4 + j;
                uint8_t t_j = get_tile(state, idx_j);
                if (t_j == 0 || (t_j - 1) / 4 != row) continue;

                if ((t_i - 1) % 4 > (t_j - 1) % 4)
                    total += 2;
            }
        }
    }

    // Linear conflict - columns
    for (int col = 0; col < 4; col++) {
        for (int i = 0; i < 4; i++) {
            int idx_i = i * 4 + col;
            uint8_t t_i = get_tile(state, idx_i);
            if (t_i == 0 || (t_i - 1) % 4 != col) continue;

            for (int j = i + 1; j < 4; j++) {
                int idx_j = j * 4 + col;
                uint8_t t_j = get_tile(state, idx_j);
                if (t_j == 0 || (t_j - 1) % 4 != col) continue;

                if ((t_i - 1) / 4 > (t_j - 1) / 4)
                    total += 2;
            }
        }
    }

    return (uint8_t)total;
}
