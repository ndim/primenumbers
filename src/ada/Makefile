EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)

primenumbers: primenumbers.adb
	gnatmake $(CFLAGS) $<

include ../../benchmark.mk
