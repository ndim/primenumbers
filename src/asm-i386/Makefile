EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)

primenumbers: primenumbers.o glue.o
primenumbers.o: primenumbers.s primenumbers.h
glue.o: glue.c primenumbers.h

include ../../benchmark.mk
