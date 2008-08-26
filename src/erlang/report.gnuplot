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
	"report.txt" using ($1):(0.001*$2) title "p4" with linespoints, \
	"report.txt" using ($1):(0.001*$3) title "p5" with linespoints, \
	"report.txt" using ($1):(0.001*$4) title "p6" with linespoints, \
	"report.txt" using ($1):(0.001*$5) title "p7" with linespoints, \
	"report.txt" using ($1):(0.001*$6) title "prime_sieve_ack_flex" with linespoints, \
	"report.txt" using ($1):(0.001*$7) title "prime_sieve_circle" with linespoints, \
	"report.txt" using ($1):(0.001*$8) title "prime_sieve_circle2" with linespoints
