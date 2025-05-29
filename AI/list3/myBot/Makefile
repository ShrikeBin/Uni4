CXX = clang++
CXXFLAGS = -std=c++17 -Wall -I ./include

SRC = ./core/bot.cpp
OUTDIR = ./bin
OUT = $(OUTDIR)/bot

all: $(OUTDIR) 
	$(CXX) $(CXXFLAGS) -o $(OUT) $(SRC)

$(OUTDIR):
	mkdir -p $(OUTDIR)

clean:
	rm -f $(OUT)
