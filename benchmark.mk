PATH := .:$(VPATH):$(PATH)

.PHONY: benchmark
benchmark: $(EXECUTABLE)
	$(TIME) -v "$(EXECUTABLE)" > "$(STDOUT)" 2> "$(STDERR)"
