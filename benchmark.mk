PATH := .:$(VPATH):$(PATH)

include $(COMPILE_OPTIONS)

.PHONY: benchmark
benchmark: $(EXECUTABLE)
	("$(EXECUTABLE)" > /dev/null & pid="$$!"; \
		sleep 3; kill    "$$pid"; \
		sleep 1; kill -9 "$$pid"; \
		wait "$$pid") 2>/dev/null; :
	$(TIME) -v "$(EXECUTABLE)" > "$(STDOUT).tmp" 2> "$(STDERR).tmp"
	mv -f "$(STDOUT).tmp" "$(STDOUT)"
	mv -f "$(STDERR).tmp" "$(STDERR)"
