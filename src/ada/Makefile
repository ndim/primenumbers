EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)
	date > compile-stamp

primenumbers: primenumbers.adb
	gnatmake $(CFLAGS) $<

include ../../benchmark.mk
