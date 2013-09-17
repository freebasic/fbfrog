#!/usr/bin/make -f
#
# This runs fbfrog on all the .h files in this directory, and captures its
# output into corresponding .txt files.
#

FBFROG := ../../fbfrog
FBC := fbc

H := $(wildcard *.h)
TXT := $(patsubst %.h,%.txt,$(H))

ifndef V
  QUIET = @echo "TEST $<";
endif

.SUFFIXES:

.PHONY: all
all: $(TXT)

$(TXT): %.txt: %.h force-rebuild
	$(QUIET)$(FBFROG) $< > $@
ifneq (,$(filter dos win32,$(shell $(FBC) -print host)))
	@unix2dos -q $@
endif

.PHONY: force-rebuild
force-rebuild:
