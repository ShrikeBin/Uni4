#include "disjointDB.hpp"

int main() 
{
    DisjointPatternDB db;
    db.build();
    db.save("disjoint_");

    return 0;
}