EXECUTABLE = primenumbers

.PHONY: build
build:
	if test ! -f compile-stamp || \
		test $(EXECUTABLE) -nt compile-stamp; then \
		date > compile-stamp; \
	fi

include ../../benchmark.mk
