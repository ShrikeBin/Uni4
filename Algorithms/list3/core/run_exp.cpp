#include "exp.hpp"
#include <iostream>
#include <unordered_map>

int main(int argc, char* argv[]) 
{
    if (argc != 2) 
    {
        std::cerr << "Usage: " << argv[0] << " <k>\n";
        return 1;
    }

    int k = std::stoi(argv[1]);

    if(k <= 0) 
    {
        std::cerr << "k must be greater than 0." << std::endl;
        return 1;
    }

    run_full(k);

    return 0;
}
