SOURCES :=	src/main.ml
RESULT :=	a.out

all: native-code

input: all
	time ./$(RESULT) input.txt

test: all
	time ./$(RESULT) test.txt

re: clean all

.PHONY: all test run re
-include OCamlMakefile
