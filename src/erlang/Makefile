EXECUTABLE = run-erlang.sh

.PHONY: build
build: p4.beam p41.beam prime_sieve_ack.beam prime_sieve_circle3.beam matthavener.beam
	for beam in $^; do \
		if test ! -f compile-stamp || \
		   test "$${beam}" -nt compile-stamp; then \
			date > compile-stamp; \
		fi; \
	done

include ../../benchmark.mk

%.beam: %.erl
	erlc $<
