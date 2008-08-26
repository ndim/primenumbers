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

reports: $(foreach x,eps svg png,report.$(x))

report.eps: report.txt report.gnuplot Makefile
	(echo "set terminal postscript eps"; \
	 echo "set output \"report.eps\""; \
         cat report.gnuplot) | gnuplot -

report.svg: report.txt report.gnuplot Makefile
	(echo "set terminal svg size 800,600 dynamic"; \
	 echo "set output \"report.svg\""; \
         cat report.gnuplot) | gnuplot -

report.png: report.txt report.gnuplot Makefile
	(echo "set terminal png giant size 800,600"; \
	 echo "set output \"report.png\""; \
         cat report.gnuplot) | gnuplot -
