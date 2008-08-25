set datafile separator "\t"

set logscale x
set logscale y

plot \
	"report.txt" using ($1):($2) title "p4" with linespoints, \
	"report.txt" using ($1):($3) title "p5" with linespoints, \
	"report.txt" using ($1):($4) title "p6" with linespoints, \
	"report.txt" using ($1):($5) title "p7" with linespoints, \
	"report.txt" using ($1):($6) title "prime_sieve_ack_flex" with linespoints, \
	"report.txt" using ($1):($7) title "prime_sieve_circle" with linespoints
