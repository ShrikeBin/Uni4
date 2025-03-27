all:
	mkdir -p ./app
	mkdir -p ./results
	clang++ core/gen.cpp -o ./app/gen
	clang++ -I./include source/*.cpp core/run_exp.cpp -o ./app/run_exp
	clang++ -I./include source/*.cpp core/sort.cpp -o ./app/sort

# Clean target: remove binaries
clean:
	rm -rf ./app

.PHONY: all clean
