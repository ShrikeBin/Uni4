all:
	mkdir -p ./app
	mkdir -p ./results
	clang++ -I./include source/*.cpp core/main.cpp -o ./app/main

# Clean target: remove binaries
clean:
	rm -rf ./app

.PHONY: all clean
