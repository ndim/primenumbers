set title "Runtimes of different Erlang programs generating the first n prime numbers"

set logscale x
set logscale y
#set xrange [10:100000]
#set yrange [0.01:10000]

set xlabel "number of prime numbers calculated"
set ylabel "run time in seconds"

set key left top box

set grid xtics ytics

set datafile separator "\t"

# FIXME: Needs to be kept in sync with report.txt
plot \
	"report-p4.txt" using ($1):(0.001*$2) title "p4" with linespoints lw 2, \
	"report-p5.txt" using ($1):(0.001*$2) title "p5" with linespoints lw 2, \
	"report-prime_sieve_ack_flex.txt" using ($1):(0.001*$2) title "prime_sieve_ack_flex" with linespoints lw 2, \
	"report-prime_sieve_circle.txt" using ($1):(0.001*$2) title "prime_sieve_circle" with linespoints lw 2, \
	"report-prime_sieve_circle3.txt" using ($1):(0.001*$2) title "prime_sieve_circle3" with linespoints lw 2
