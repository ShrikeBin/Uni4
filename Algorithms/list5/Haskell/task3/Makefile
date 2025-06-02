# Makefile

TARGET=heaptest
SRC=BinHeap.hs HeapTest.hs Main.hs

all: $(TARGET)

$(TARGET): $(SRC)
	stack ghc -- $(SRC) -o $(TARGET) -package random

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET) *.hi *.o

.PHONY: all run clean
