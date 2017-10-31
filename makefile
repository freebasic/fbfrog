FBC := fbc
FBFROG := $(shell $(FBC) -m fbfrog -print x)
TESTSRUN := $(shell $(FBC) tests/run.bas -print x)
prefix := /usr/local

-include config.mk

ALLFBCFLAGS := -m fbfrog -maxerr 1 $(FBFLAGS)
ALLFBLFLAGS := $(FBFLAGS)

SOURCES := $(sort $(wildcard src/*.bas))
HEADERS := $(wildcard src/*.bi)
OBJECTS := $(patsubst src/%.bas,src/obj/%.o,$(SOURCES))

LLVM_LIBDIR := $(shell llvm-config-4.0 --libdir)
LIBS := -p $(LLVM_LIBDIR) -l clang

# We don't want to use any of make's built-in suffixes/rules
.SUFFIXES:

ifndef V
  QUIET_FBC     = @echo "FBC $<";
  QUIET_FBCLINK = @echo "FBCLINK $@";
endif

build: $(FBFROG) $(TESTSRUN)

$(FBFROG): $(OBJECTS)
	$(QUIET_FBCLINK)$(FBC) $(ALLFBLFLAGS) $^ $(LIBS) -x $@

$(OBJECTS): src/obj/%.o: src/%.bas $(HEADERS)
	$(QUIET_FBC)$(FBC) $(ALLFBCFLAGS) $< -c -o $@

$(TESTSRUN): tests/run.bas src/util-path.bas src/util-str.bas
	$(QUIET_FBCLINK)$(FBC) $< $(FBFLAGS)

tests: build
	$(TESTSRUN)

clean:
	rm -f $(FBFROG) $(TESTSRUN) src/obj/*.o

install:
	install $(FBFROG) "$(prefix)/bin"
	cp -R include/fbfrog "$(prefix)/include"

COMMIT = $(shell git rev-parse --verify HEAD)
tarball:
	git archive --format tar --prefix "fbfrog-$(COMMIT)/" $(COMMIT) | xz --stdout > fbfrog-$(COMMIT).tar.xz

.PHONY: all tests clean install tarball
