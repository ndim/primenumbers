#!/bin/sh
# erl -noshell -s primenumbers main -s init stop

for module in prime2; do
	if [ "${module}.erl" -nt "${module}.beam" ]; then
		erl -compile "${module}"
	fi
done

erl -noshell -s prime2 oerks -s init stop
