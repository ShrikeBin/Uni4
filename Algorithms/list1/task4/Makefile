CC = clang++
CFLAGS = -Wall -Wextra -std=c++17
TARGET = main
SRC = main.cpp
INCLUDE = $(wildcard *.hpp)

all: $(TARGET)

$(TARGET): $(SRC) $(INCLUDE)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)
