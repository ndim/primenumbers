EXECUTABLE = primenumbers

.PHONY: build
build: $(EXECUTABLE)

ifdef USE_JAVA_VM

primenumbers: primenumbers.class
	echo -e "#!/bin/sh\n$(JAVA_HOME)/bin/java -cp \$$(dirname \$$0) primenumbers" > $@
	chmod +x $@

primenumbers.class: primenumbers.java
	$(JAVA_HOME)/bin/javac $(JAVAC_FLAGS) -d . $<

else

primenumbers: primenumbers.java
	gcj $(CFLAGS) --main=primenumbers -o $@ $<

endif

include ../../benchmark.mk
