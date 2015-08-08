FBC := fbc
FBFROG := $(shell $(FBC) -m fbfrog -print x)
TESTSRUN := $(shell $(FBC) tests/run.bas -print x)
prefix := /usr/local

-include config.mk

build: $(FBFROG) $(TESTSRUN)

$(FBFROG): $(wildcard *.bas *.bi)
	$(FBC) *.bas -m fbfrog -maxerr 1 $(FBFLAGS)

$(TESTSRUN): tests/run.bas util-path.bas util-str.bas
	$(FBC) $< $(FBFLAGS)

tests: build
	$(TESTSRUN)

clean:
	rm -f $(FBFROG) $(TESTSRUN)

install:
	install $(FBFROG) "$(prefix)/bin"
	cp -R include/fbfrog "$(prefix)/include"

.PHONY: all tests clean install
