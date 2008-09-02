EXECUTABLE = primenumbers

.PHONY: build
build:
	sed 's/\ts =/\t##s =/; s/\t#s =/\ts =/;' < ../../src/awk/primenumbers > primenumbers; \
	chmod +x primenumbers; \
	if test ! -f compile-stamp || \
		test $(EXECUTABLE) -nt compile-stamp; then \
		date > compile-stamp; \
	fi

include ../../benchmark.mk
