# Guess HOST if not set
ifndef HOST
  # 'uname' tells the system's name. On Linux, it returns 'Linux',
  # DJGPP's uname returns 'MS-DOS', and MinGW's uname returns something
  # like 'MINGW32_NT-5.1'.
  uname := $(shell uname)
  ifneq ($(findstring MINGW,$(uname)),)
    HOST := win32
  else ifeq ($(uname),Linux)
    HOST := linux
  else ifeq ($(uname),MS-DOS)
    HOST := dos
  else
    $(error Unrecognized uname: '$(uname)')
  endif
endif

ifneq ($(filter win32 dos,$(HOST)),)
  EXEEXT := .exe
endif

FBC := fbc
FROG := ./fbfrog$(EXEEXT)

ifndef V
  QUIET_TEST    = @echo "TEST $@";
endif

.PHONY: all
all: $(FROG)

$(FROG): $(wildcard *.bas)
	$(FBC) -exx $^ -x $@

.PHONY: clean
clean:
	-rm -f $(FROG)

TESTSH := $(wildcard tests/*.h)
TESTSBI := $(patsubst %.h,%.bi,$(TESTSH))

.PHONY: FORCE
FORCE:

.PHONY: tests
tests: $(TESTSBI)

$(TESTSBI): %.bi: %.h FORCE
	$(QUIET_TEST)$(FROG) $<
