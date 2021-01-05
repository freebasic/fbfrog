FBC := fbc
EXEEXT := $(shell $(FBC) -print x)
prefix := /usr/local

-include config.mk

FBFROG_FBFLAGS := -m fbfrog -maxerr 1 $(FBFLAGS)

SOURCES := $(sort $(wildcard src/*.bas))
HEADERS := $(wildcard src/*.bi)
OBJECTS := $(patsubst src/%.bas,src/obj/%.o,$(SOURCES))

UNIT_TESTS := tests/unit/string-is-valid-id$(EXEEXT)
UNIT_TESTS += tests/unit/string-matching$(EXEEXT)

# We don't want to use any of make's built-in suffixes/rules
.SUFFIXES:

ifndef V
  QUIET_FBC     = @echo "FBC $<";
  QUIET_FBCLINK = @echo "FBCLINK $@";
endif

build: fbfrog$(EXEEXT) tests/run$(EXEEXT) $(UNIT_TESTS)

fbfrog$(EXEEXT): $(OBJECTS)
	$(QUIET_FBCLINK)$(FBC) $(FBFROG_FBFLAGS) $^ -x $@

$(OBJECTS): src/obj/%.o: src/%.bas $(HEADERS)
	$(QUIET_FBC)$(FBC) $(FBFROG_FBFLAGS) $< -c -o $@

tests/run$(EXEEXT): tests/run.bas src/obj/util-path.o src/obj/util-str.o
	$(QUIET_FBCLINK)$(FBC) $^ $(FBFLAGS)

$(UNIT_TESTS): tests/unit/%$(EXEEXT): tests/unit/%.bas tests/unit/test src/obj/util-str.o
	$(QUIET_FBCLINK)$(FBC) $< src/obj/util-str.o $(FBFLAGS)

tests: build
	tests/run$(EXEEXT)

clean:
	rm -f fbfrog$(EXEEXT) tests/run$(EXEEXT) $(UNIT_TESTS) src/obj/*.o

install:
	install fbfrog$(EXEEXT) "$(prefix)/bin"
	cp -R include/fbfrog "$(prefix)/include"

COMMIT = $(shell git rev-parse --verify HEAD)
tarball:
	git archive --format tar --prefix "fbfrog-$(COMMIT)/" $(COMMIT) | xz --stdout > fbfrog-$(COMMIT).tar.xz

.PHONY: all tests clean install tarball
