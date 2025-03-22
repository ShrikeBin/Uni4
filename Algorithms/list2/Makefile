# Compiler and flags
CXX = clang++
CXXFLAGS = -std=c++17 -Wall -Wextra

# Source files
SOURCES = generate.cpp sort.cpp

# Output files (binaries)
TARGETS = generate sort

# Default target: compile everything
all: $(TARGETS)

# Rule for compiling generate.cpp
generate: generate.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

# Rule for compiling sort.cpp
sort: sort.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

# Clean target: remove binaries
clean:
	rm -f $(TARGETS)

.PHONY: all clean
