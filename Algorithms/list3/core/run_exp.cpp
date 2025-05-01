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

    return 0;
}
