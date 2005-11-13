EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)
	date > compile-stamp

primenumbers: primenumbers.o glue.o
primenumbers.o: primenumbers.s primenumbers.h
glue.o: glue.c primenumbers.h

include ../../benchmark.mk
