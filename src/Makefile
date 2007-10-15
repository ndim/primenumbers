TESTCASEDIR = ../testcases
BUILDDIR = ../build
RESULTDIR = ../results

.PHONY: build
build:
	@for tc in $(TESTCASEDIR)/*.tc; \
	do \
		test -f "$$tc" || exit 101; \
		name="$$(basename "$${tc}" .tc)"; \
		title="$$(head -n1 $${tc})"; \
		src="$$(head -n2 $${tc} | tail -n1)"; \
		echo "############################################################"; \
		echo "# Name:   $${name}"; \
		echo "# Title:  $${title}"; \
		echo "# Source: $${src}"; \
		echo "############################################################"; \
		srcdir="$$(cd $$src && pwd)" || exit 102; \
		builddir="$(BUILDDIR)/$$name"; \
		mkdir -p "$$builddir" || exit 103; \
		ln -sf "$$srcdir/Makefile" "$$builddir/Makefile" || exit 104; \
		( ifs="$$IFS"; IFS="=$$IFS"; \
		n=-2; while read var value; do \
			n=$$(($$n+1)); \
			[ $$n -le 0 ] && continue; \
			echo "[$$var] = [$$value]"; \
			export $$var="$$value"; \
		done < "$$tc"; \
		IFS="$$ifs"; \
		VPATH="$$srcdir" $(MAKE) -C "$$builddir" "$@" ); \
		status="$$?"; \
		if test "$$status" -ne 0; then \
			failed="$${failed} $${name}"; \
		fi; \
	done; \
	if test "x$$failed" = "x"; then \
		echo "All test case builds succeeded."; \
	else \
		echo -e "Building for some test cases failed:\n   $${failed}"; \
		exit 105; \
	fi

.PHONY: benchmark
benchmark: build
	@for tc in $(TESTCASEDIR)/*.tc; \
	do \
		[ -f "$$tc" ] || exit 101; \
		name="$$(basename "$${tc}" .tc)"; \
		title="$$(head -n1 $${tc})"; \
		echo "$$title" | grep "^DISABLED " > /dev/null && continue; \
		src="$$(head -n2 $${tc} | tail -n1)"; \
		mkdir -p "$(RESULTDIR)" || exit 102; \
		resultdir="$$(cd "$(RESULTDIR)" && pwd)" || exit 103; \
		(echo "############################################################"; \
		echo "# Name:   $${name}"; \
		echo "# Title:  $${title}"; \
		echo "# Source: $${src}"; \
		echo "# Date:   $$(TZ=UTC date)"; \
		echo "############################################################") > "$${resultdir}/$${name}.info"; \
		srcdir="$$(cd $$src && pwd)" || exit 104; \
		builddir="$(BUILDDIR)/$$name"; \
		if test "$${builddir}/compile-stamp" -nt "$${resultdir}/$${name}.time"; then :; else continue; fi; \
		(read;read;cat) < "$$tc" > "$${resultdir}/$${name}.compile-options"; \
		$(MAKE) COMPILE_OPTIONS="$${resultdir}/$${name}.compile-options" \
			VPATH="$$srcdir" \
			STDOUT="$${resultdir}/$${name}.output" \
			STDERR="$${resultdir}/$${name}.time" \
			-C "$$builddir" "$@" < /dev/null; \
		status="$$?"; \
		if test "$$status" -ne 0; then \
			failed="$${failed} $${name}"; \
		fi; \
		(cat "$${resultdir}/$${name}.info" "$${resultdir}/$${name}.compile-options"; \
		head -n5 "$${resultdir}/$${name}.time" || exit 105; \
		echo "############################################################") > "$${resultdir}/$${name}.report" || exit "$$?" ; \
		cat "$${resultdir}/$${name}.report"; \
	done; \
	if test "x$$failed" = "x"; then \
		echo "All test cases succeeded."; \
	else \
		echo -e "Some test cases failed:\n   $${failed}"; \
		exit 106; \
	fi

