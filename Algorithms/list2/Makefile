# Compiler and flags
CXX = clang++
CXXFLAGS = -I./include

# Directories
BUILD_DIR = ./app

# Source files
MAIN_SRC = core/main.cpp
SORTS_SRC = source/sorts.cpp
GEN_SRC = core/gen.cpp

# Output files
TARGET_MAIN = $(BUILD_DIR)/main
TARGET_GEN = $(BUILD_DIR)/gen

# Default target: compile main executable
all: $(TARGET_MAIN) $(TARGET_GEN)

# Rule for compiling main executable
$(TARGET_MAIN): $(MAIN_SRC) $(SORTS_SRC) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Rule for compiling gen executable
$(TARGET_GEN): $(GEN_SRC) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Ensure the build directory exists
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Clean target: remove binaries
clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean
