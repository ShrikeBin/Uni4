#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

void generate_random(int n)
{
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<int> dist(0, 2 * n - 1);
    
    for (int i = 0; i < n; ++i) 
    {
        std::cout << dist(mt) << " ";
    }
    std::cout << "\n";
}

void generate_ascending(int n) 
{
    for (int i = 0; i < n; ++i) 
    {
        std::cout << i << " ";
    }
    std::cout << "\n";
}

void generate_descending(int n) 
{
    for (int i = n - 1; i >= 0; --i) 
    {
        std::cout << i << " ";
    }
    std::cout << "\n";
}

int main(int argc, char* argv[]) 
{
    if (argc != 3) 
    {
        std::cerr << "Usage: " << argv[0] << " <mode> <n>\n";
        return -1;
    }
    
    std::string mode = argv[1];
    int n = std::stoi(argv[2]);
    
    if (mode == "random") 
    {
        generate_random(n);
    } 
    else if (mode == "asc") 
    {
        generate_ascending(n);
    } 
    else if (mode == "desc") 
    {
        generate_descending(n);
    } 
    else
    {
        std::cerr << "Invalid mode. Use 'random', 'asc', or 'desc'.\n";
        return -1;
    }
    
    return 0;
}
