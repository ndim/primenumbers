PATH := .:$(VPATH):$(PATH)

include $(COMPILE_OPTIONS)

MAX = 262144
MAX = 131072
MAX = 65536
# MAX = 1000
# MAX = 100
# MAX =

# This first starts up the executable and then kills it a few seconds
# later. That should ensure all the data loaded from disk is in the
# cachen when the actual timed run starts.
.PHONY: benchmark
benchmark: $(EXECUTABLE)
	ulimit -t 2; "$(EXECUTABLE)" $(MAX) > /dev/null 2>&1; :
	set -ex; \
	repeats=1; \
	while :; do \
	  begin_ts="$$(date +%s)"; \
	  export repeats; \
	  $(TIME) -v sh -ex -c 'n=0; while test "$$n" -lt "$$repeats"; do "$(EXECUTABLE)" $(MAX) > "$(STDOUT).tmp"; n="$$(expr "$$n" + 1)"; done' 2> "$(STDERR).tmp"; \
	  end_ts="$$(date +%s)"; \
	  elapsed="$$(expr "$$end_ts" - "$$begin_ts" ||:)"; \
	  if test "$$elapsed" -ge 4; then \
	    echo repeats="$$repeats" > "$(REPEATS).tmp"; \
	    break; \
	  fi; \
	  repeats="$$(expr "$$repeats" '*' 5)"; \
	done
	mv -f "$(REPEATS).tmp" "$(REPEATS)"
	mv -f "$(STDOUT).tmp" "$(STDOUT)"
	mv -f "$(STDERR).tmp" "$(STDERR)"
