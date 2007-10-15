EXECUTABLE = run-erlang.sh

.PHONY: build
build: p4.beam prime_sieve_ack.beam
	for beam in $^; do \
		if test ! -f compile-stamp || \
		   test "$${beam}" -nt compile-stamp; then \
			date > compile-stamp; \
		fi; \
	done

include ../../benchmark.mk

%.beam: %.erl
	erlc $<
