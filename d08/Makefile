SOURCES =	src/main.ml

RESULT =	a.out

all: native-code

input: all
	@echo ""
	time -p ./$(RESULT) input.txt 1000

test: all
	@echo ""
	time -p ./$(RESULT) test.txt 10


re: clean all

.PHONY: all test run re
-include OCamlMakefile
