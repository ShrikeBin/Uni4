all:
	mkdir -p ./app
	clang++ core/gen.cpp -o ./app/gen
	clang++ -I./include source/*.cpp core/main.cpp -o ./app/main

# Clean target: remove binaries
clean:
	rm -rf ./app

.PHONY: all clean
