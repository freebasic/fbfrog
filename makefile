FBC := fbc
FBFROG := $(shell $(FBC) -m fbfrog -print x)
TESTSRUN := $(shell $(FBC) tests/run.bas -print x)

-include config.mk

build: $(FBFROG) $(TESTSRUN)

$(FBFROG): $(wildcard *.bas *.bi)
	$(FBC) *.bas -m fbfrog $(FBFLAGS)

$(TESTSRUN): tests/run.bas util-path.bas util-str.bas
	$(FBC) $< $(FBFLAGS)

tests: build
	$(TESTSRUN)

clean:
	rm -f $(FBFROG) $(TESTSRUN)

.PHONY: all tests clean
