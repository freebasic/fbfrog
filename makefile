#!/usr/bin/make -f
#
# Pass V=1 to see more verbose command lines.
# Use FBFLAGS='...' to override the default debug build.
#

CP := cp
RM := rm -f
FBC := fbc
FBFLAGS := -g -exx
EXEEXT :=

HEADERS := common.bi

OBJECTS := main.o

#
# Host specific configuration
#

uname_s := $(shell uname -s || echo unknown)
ifneq ($(findstring MINGW,$(uname_s)),)
	EXEEXT := .exe
endif

ifndef V
	QUIET_FBC  = @echo "	" FBC $@;
	QUIET_LINK = @echo "	" LINK $@;
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

.PHONY: clean
clean:
	$(RM) $(FROG_EXE) $(OBJECTS)
