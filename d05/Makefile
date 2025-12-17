SOURCES =	src/main.ml

RESULT =	a.out

all: native-code

input: all
	echo ""
	time -p ./$(RESULT) input.txt

test: all
	echo ""
	time -p ./$(RESULT) test.txt

re: clean all

.PHONY: all test run re
-include OCamlMakefile
