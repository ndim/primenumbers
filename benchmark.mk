PATH := .:$(VPATH):$(PATH)

include $(COMPILE_OPTIONS)

.PHONY: benchmark
benchmark: $(EXECUTABLE)
	$(TIME) -v "$(EXECUTABLE)" > "$(STDOUT).tmp" 2> "$(STDERR).tmp"
	mv -f "$(STDOUT).tmp" "$(STDOUT)"
	mv -f "$(STDERR).tmp" "$(STDERR)"
