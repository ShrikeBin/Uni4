all:
	mkdir -p ./app
	mkdir -p ./results
	clang++ core/gen.cpp -o ./app/gen
	clang++ -I./include source/*.cpp core/main.cpp -o ./app/main
	clang++ -I./include source/*.cpp core/run_exp.cpp -o ./app/run_exp

# Clean target: remove binaries
clean:
	rm -rf ./app

.PHONY: all clean
