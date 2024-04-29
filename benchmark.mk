PATH := .:$(VPATH):$(PATH)

include $(COMPILE_OPTIONS)

MAX = 262144
MAX = 65536
# MAX = 1000
# MAX =

# This first starts up the executable and then kills it a few seconds
# later. That should ensure all the data loaded from disk is in the
# cachen when the actual timed run starts.
.PHONY: benchmark
benchmark: $(EXECUTABLE)
	ulimit -t 3; "$(EXECUTABLE)" > /dev/null 2>&1; :
	set -x; $(TIME) -v "$(EXECUTABLE)" $(MAX) > "$(STDOUT).tmp" 2> "$(STDERR).tmp"
	mv -f "$(STDOUT).tmp" "$(STDOUT)"
	mv -f "$(STDERR).tmp" "$(STDERR)"
