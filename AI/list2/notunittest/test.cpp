#include <iostream>
#include <iomanip>
#include <cstdint>
#include <array>
#include <fstream>
#include <bitset>


void print_state_and_cost(const uint32_t entry) 
{
    // Extract the heuristic cost (rightmost 8 bits)
    uint8_t heuristic_cost = entry & 0xFF;

    // Extract the positions of tiles (the remaining 24 bits)
    uint32_t positions = entry >> 12;

    std::cout << "Heuristic Cost: " << (int)heuristic_cost << "\nState:\n";

    // Create an array to hold the positions of the tiles in a 4x4 grid
    std::array<uint8_t, 16> state;
    
    // Initially, set all positions to 0xFF (representing empty spots)
    state.fill(0xFF);

    // Fill in the state based on the positions encoded in the 24 bits
    for (int i = 0; i < 5; ++i) 
    {
        int pos = (positions >> (4 * (4 - i)) & 0xF);  // Extract the position of tile i (0-4)
        if (pos < 16) 
        {  // Ensure valid position (within the 4x4 grid)
            state[pos] = i + 1;  // Place the tile number (1-5)
        }
    }

    // Print the state as a 4x4 grid
    for (int i = 0; i < 16; ++i) {
        if (i % 4 == 0) std::cout << "\n";  // New line every 4 elements for grid format
        if (state[i] == 0xFF)
            std::cout << " . ";  // Representing empty spots (0xFF) as a dot
        else
            std::cout << std::setw(2) << std::setfill(' ') << (int)state[i] << " ";
    }

    std::cout << "\n\n";
}

void read_entries_from_file(const std::string& filename, size_t num_entries) {
    // Open the binary file in read mode
    std::ifstream infile(filename, std::ios::binary);
    
    // Check if the file was opened successfully
    if (!infile) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return;
    }

    uint32_t entry;
    size_t count = 0;

    // Read the specified number of 32-bit entries from the file
    while (count < num_entries && infile.read(reinterpret_cast<char*>(&entry), sizeof(entry))) {
        // Print the entry (you can also process it or print the state and cost as needed)
        std::cout << "Entry " << count + 1 << ": " << std::bitset<32>(entry) << std::endl;
        
        print_state_and_cost(entry);

        count++;
    }

    // If we haven't read enough entries, notify the user
    if (count < num_entries) {
        std::cerr << "Not enough entries in the file. Read " << count << " entries." << std::endl;
    }

    infile.close();
}

int main() 
{
    // Example usage: Read 10 entries from a file named "pattern_1.bin"
    std::string filename = "../precompute/disjoint_pattern_1.bin";
    size_t num_entries = 10;
    read_entries_from_file(filename, num_entries);

    return 0;
}
