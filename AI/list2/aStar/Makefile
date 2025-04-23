all:
	mkdir -p ./app
	clang++ -I./include source/*.cpp core/main.cpp -o ./app/main

run:
	./app/main
	
clean:
	rm -rf ./app

.PHONY: all clean                           
