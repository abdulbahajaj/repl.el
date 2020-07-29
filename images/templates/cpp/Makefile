include /common/makefile.common

bin/main: main.cpp
	@mkdir -p bin
	g++ main.cpp -o bin/main

.PHONY: build
build: bin/main

.PHONY: run
run: build
	./bin/main
