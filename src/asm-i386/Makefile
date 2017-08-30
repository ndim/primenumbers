EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)
	date > compile-stamp

# Force 32bit compilation due to asm commands being written for 32bit.

primenumbers : LDFLAGS+=-m32
primenumbers: primenumbers.o glue.o
primenumbers.o : ASFLAGS+=-march=i586
primenumbers.o : ASFLAGS+=--32
primenumbers.o: primenumbers.s primenumbers.h
glue.o : CFLAGS+=-march=i586
glue.o : CFLAGS+=-m32
glue.o: glue.c primenumbers.h

include ../../benchmark.mk
