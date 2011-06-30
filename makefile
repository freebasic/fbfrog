#!/usr/bin/make -f
#
# Pass V=1 to see more verbose command lines.
# Use FBFLAGS='' to override the default debug build.
# Specify PREFIX='/foo/bar' to override the default /usr/local install target.

FBC := fbc
FBFLAGS := -g -exx -d ENABLE_STATS
EXEEXT :=
PREFIX := /usr/local


HEADERS :=
OBJECTS :=

HEADERS += common.bi
HEADERS += lex.bi
HEADERS += hash.bi
HEADERS += tree.bi

OBJECTS += common.o
OBJECTS += main.o
OBJECTS += lex.o
OBJECTS += hash.o
OBJECTS += tree.o


#
# Host specific configuration
#

uname_s := $(shell uname -s || echo unknown)
ifneq ($(findstring MINGW,$(uname_s)),)
	EXEEXT := .exe
endif


#
# Build rules
#

ifndef V
	QUIET_FBC  = @echo "FBC $@";
	QUIET_LINK = @echo "LINK $@";
endif

FROG_EXE := fbfrog$(EXEEXT)
FROG_FBFLAGS := -maxerr 1 -m main -include common.bi

.SUFFIXES:

.PHONY: all
all: $(FROG_EXE)

$(FROG_EXE): $(OBJECTS)
	$(QUIET_LINK)$(FBC) $(FROG_FBFLAGS) $(FBFLAGS) $^ -x $@

$(OBJECTS): %.o: %.bas $(HEADERS)
	$(QUIET_FBC)$(FBC) $(FROG_FBFLAGS) $(FBFLAGS) -c $< -o $@

.PHONY: install
install:
	mkdir -p $(PREFIX)/bin
	cp $(FROG_EXE) $(PREFIX)/bin

.PHONY: uninstall
uninstall:
	rm $(PREFIX)/bin/$(FROG_EXE)

.PHONY: clean
clean:
	rm -f $(FROG_EXE) $(OBJECTS)
