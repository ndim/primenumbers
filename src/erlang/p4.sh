#!/bin/sh

cd "$(dirname "$0")"
if test p4.erl -nt p4.beam; then erl -compile p4 || exit 13; fi
#ulimit -t 1
erl -noshell -s p4 main -s init stop
