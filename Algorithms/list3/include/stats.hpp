#ifndef STATS_HPP
#define STATS_HPP

struct Stats 
{
    unsigned long long comparisons = 0;
    unsigned long long swaps = 0;
    unsigned long long selects = 0;

    void reset() 
    {
        comparisons = 0;
        swaps = 0;
        selects = 0;
    }
};

#endif // STATS_HPP