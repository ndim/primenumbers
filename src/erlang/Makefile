EXECUTABLE = p4.sh

.PHONY: build
build: p4.beam

include ../../benchmark.mk

%.beam: %.erl
	erlc $<
