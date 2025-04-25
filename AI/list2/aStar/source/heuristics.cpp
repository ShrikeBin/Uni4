#include "heuristics.hpp"

uint8_t PatternDatabase(State state)
{
    // przypomnijmy zapis w bazie danych:
    // 32 bity:
    // pozycja 1 | pozycja 2 | pozycja 3 | pozycja 4 | pozycja 5 | 4 bity gówna | 8 bitów heurystyka
    // pozycja 6 | pozycja 7 | pozycja 8 | pozycja 9 | pozycja 10 | 4 bity gówna | 8 bitów heurystyka
    // pozycja 11 | pozycja 12 | pozycja 13 | pozycja 14 | pozycja 15 | 4 bity gówna | 8 bitów heurystyka

    // pomysł jak to zrobić:
    // masz stan 64 bity:
    // zrób getTile(state, i) i w [0-15]
    // zrób 3 tablice z pozycjami tj
    // przeleć przez cały state i dodawaj do odpowiedniej tablicy
    // po kolei ify
    // jeśli getTile(state, i) == 0 continue;
    // jeśli getTile(state, i) jest  < 6 dodaj do tablicy 1 i na miejscu [getTile(state, i) - 1]  !!
    // jeśli getTile(state, i) jest  < 11 dodaj do tablicy 2 i na miejscu [getTile(state, i) - 6] !!
    // jeśli getTile(state, i) jest else dodaj do tablicy 3 i na miejscu [getTile(state, i) - 11] !!
    // (dla 1-5) dla np odwróconego: tiles1[5] = {15, 14, 13, 12, 11}
    // dostajesz tablicę już sformatowaną po kolei(da się zamienić na to żeby to już był uint32_t)
    // wyszukaj w bazie danych (która chyba będzie unordered_map <uint32_t, uint8_t>)
    // posumuj
    // zwróć wynik

    return 0;
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
