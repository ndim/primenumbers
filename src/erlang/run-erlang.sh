#!/bin/sh

cd "$(dirname "$0")"
if test "${MODULE}.erl" -nt "${MODULE}.beam"
then
    erl -compile "${MODULE}" || exit 13
fi

erl ${ERL_OPTS} -noshell -s "${MODULE}" start "$@" -s init stop
