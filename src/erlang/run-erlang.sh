#!/bin/sh

cd "$(dirname "$0")"
if test "${MODULE}.erl" -nt "${MODULE}.beam"
then
    erl -compile "${MODULE}" || exit 13
fi
#ulimit -t 1
erl -noshell -s "${MODULE}" start -s init stop
