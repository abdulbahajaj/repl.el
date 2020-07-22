bin/main: main.c
	@mkdir -p bin
	gcc main.c -o bin/main

.PHONY: build
build: bin/main

.PHONY: run
run: build
	./bin/main
