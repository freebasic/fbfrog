FBC := fbc
EXEEXT := $(shell $(FBC) -print x)
prefix := /usr/local

-include config.mk

EXTRA_FBFLAGS := -maxerr 1
FBFROG_FBFLAGS := -m fbfrog $(FBFLAGS) $(EXTRA_FBFLAGS)
TESTRUNNER_FBFLAGS := $(FBFLAGS) $(EXTRA_FBFLAGS)
TESTRUNNER_ARGS :=

ifdef ENABLE_STANDALONE
  FBFROG_FBFLAGS += -d ENABLE_STANDALONE
endif

ifdef ENABLE_COMMON_PATHDIV
  # When ENABLE_COMMON_PATHDIV defined: 
  # - Use forward slash '/' for path division on all targets
  # - This helps with compatibility for  shell scripts.
  # When ENABLE_COMMON_PATHDIV not defined:
  # - use backslash '\' for path division on windows and dos
  # - use forward slash '/' for all other targets
  #
  # See src/util-path.bi: HOST_PATHDIV and PATHDIV

  FBFROG_FBFLAGS += -d ENABLE_COMMON_PATHDIV
  TESTRUNNER_FBFLAGS += -d ENABLE_COMMON_PATHDIV 
endif

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
	tests/run$(EXEEXT) $(TESTRUNNER_ARGS)

unittests: build
	unittests/run$(EXEEXT)

clean:
	rm -f fbfrog$(EXEEXT) tests/run$(EXEEXT) unittests/run$(EXEEXT) src/obj/*.a src/obj/*.o

install:
ifdef ENABLE_STANDALONE
	install fbfrog$(EXEEXT) "$(prefix)"
	mkdir -p "$(prefix)/fbfrog/include"
	cp -R include/fbfrog/* "$(prefix)/fbfrog/include"
else
	install fbfrog$(EXEEXT) "$(prefix)/bin"
	cp -R include/fbfrog "$(prefix)/include"
endif

COMMIT = $(shell git rev-parse --verify HEAD)
tarball:
	git archive --format tar --prefix "fbfrog-$(COMMIT)/" $(COMMIT) | xz --stdout > fbfrog-$(COMMIT).tar.xz

.PHONY: all tests clean install tarball
