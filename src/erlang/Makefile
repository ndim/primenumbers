EXECUTABLE = primenumbers.sh

.PHONY: build
build: primenumbers.beam

include ../../benchmark.mk

%.beam: %.erl
	erl -compile $*
