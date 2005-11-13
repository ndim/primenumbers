EXECUTABLE = p4.sh

.PHONY: build
build: p4.beam
	if test ! -f compile-stamp || \
		test p4.beam -nt compile-stamp; then \
		date > compile-stamp; \
	fi

include ../../benchmark.mk

%.beam: %.erl
	erlc $<
