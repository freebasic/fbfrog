FBC := fbc
EXEEXT := $(shell $(FBC) -print x)
prefix := /usr/local

-include config.mk

EXTRA_FBFLAGS := -maxerr 1
FBFROG_FBFLAGS := -m fbfrog $(FBFLAGS) $(EXTRA_FBFLAGS)
TESTRUNNER_FBFLAGS := $(FBFLAGS) $(EXTRA_FBFLAGS)

SOURCES := $(sort $(wildcard src/*.bas))
HEADERS := $(wildcard src/*.bi)
OBJECTS := $(patsubst src/%.bas,src/obj/%.o,$(SOURCES))

# We don't want to use any of make's built-in suffixes/rules
.SUFFIXES:

ifndef V
  QUIET_FBC     = @echo "FBC $<";
  QUIET_FBCLIB  = @echo "FBCLIB $@";
  QUIET_FBCLINK = @echo "FBCLINK $@";
endif

build: fbfrog$(EXEEXT) tests/run$(EXEEXT) unittests/run$(EXEEXT)

fbfrog$(EXEEXT): src/obj/libfbfrog.a
	$(QUIET_FBCLINK)$(FBC) $(FBFROG_FBFLAGS) $^ -x $@

src/obj/libfbfrog.a: $(OBJECTS)
	$(QUIET_FBCLIB)$(FBC) $(FBFROG_FBFLAGS) -lib $^ -x $@

$(OBJECTS): src/obj/%.o: src/%.bas $(HEADERS)
	$(QUIET_FBC)$(FBC) $(FBFROG_FBFLAGS) $< -c -o $@

tests/run$(EXEEXT): tests/run.bas src/obj/libfbfrog.a
	$(QUIET_FBCLINK)$(FBC) $^ $(TESTRUNNER_FBFLAGS) -x $@

UNITTESTS_HEADERS := $(HEADERS) $(wildcard unittests/*.bi)
unittests/run$(EXEEXT): unittests/run.bas src/obj/libfbfrog.a $(UNITTESTS_HEADERS)
	$(QUIET_FBCLINK)$(FBC) $< src/obj/libfbfrog.a $(TESTRUNNER_FBFLAGS) -x $@

tests: build unittests
	tests/run$(EXEEXT)

unittests: build
	unittests/run$(EXEEXT)

clean:
	rm -f fbfrog$(EXEEXT) tests/run$(EXEEXT) unittests/run$(EXEEXT) src/obj/*.a src/obj/*.o

install:
	install fbfrog$(EXEEXT) "$(prefix)/bin"
	cp -R include/fbfrog "$(prefix)/include"

COMMIT = $(shell git rev-parse --verify HEAD)
tarball:
	git archive --format tar --prefix "fbfrog-$(COMMIT)/" $(COMMIT) | xz --stdout > fbfrog-$(COMMIT).tar.xz

.PHONY: all tests clean install tarball
