EXECUTABLE = primenumbers

.PHONY: build
build:
	sed 's/= sqrt/= math.sqrt/;' < ../../src/python/primenumbers > primenumbers; \
	chmod +x primenumbers; \
	if test ! -f compile-stamp || \
		test $(EXECUTABLE) -nt compile-stamp; then \
		date > compile-stamp; \
	fi

include ../../benchmark.mk
