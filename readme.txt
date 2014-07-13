
  fbfrog -- FreeBASIC binding generator
  Copyright (C) 2011 - 2014  Daniel C. Klauer <daniel.c.klauer[at]web.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.


What's this?

  fbfrog is a tool that reads *.h files (C API declarations) and generates a
  corresponding *.bi file (FreeBASIC API declarations). It's intended to
  automate most of the work needed to create and maintain FB bindings for C
  libraries.

  C and FB are similar enough to allow most declarations to be converted 1:1 by
  doing a pure syntax conversion, for example:
      struct UDT {              =>    type UDT
          float f;              =>        f as single
      };                        =>    end type
      void f(struct UDT *p);    =>    declare sub f(byval p as UDT ptr)

  Besides that, fbfrog performs the following "high-level" transformations:
   * C types + size_t/int32_t/... => FB types
   * char => byte, char* => zstring ptr, char[N] => zstring * N
   * Most used calling convention => Extern block
   * Other calling conventions (if header uses multiple ones) => emitted on
     individual procedures
   * Function/array typedefs (not supported in FB) => solved out
   * Array parameters (not supported in FB) => pointers
   * Anonymous structs (not supported in FB) => named after first typedef that
     uses them, or auto-generated name
   * typedef struct FOO FOO; => solved out (FB doesn't have separate types/tags)
   * #defines with simple constant expression in their bodies => FB constants
   * #defines nested inside struct bodies => moved to toplevel (helps when
     converting #defines to constants, because FB scopes those inside UDTs)
   * #define m(a, ...) __VA_ARGS__ => #define m(a, __VA_ARGS__...) __VA_ARGS__
   * Symbol name conflicts with FB keywords or each-other, for example due to
     FB's case insensitivity, => automatically renamed by appending _
     underscores to the less important symbol
   * Renamed variables/procedures => ALIAS "<original-name>" will be emitted
   * Named enum => type enumname as long + anonymous enum
     (C enums/ints stay 32bit on 64bit, so in FB we have to use the always-32bit
     LONG type instead of the default ENUM/INTEGER type)

  fbfrog has its own C pre-processor (CPP) and C parser capable of parsing most
  declarations. The C declarations (and also #defines if simple enough) are
  loaded into an AST, adjusted to become FB-friendly, and emitted as FB code.
  Parser limitations:
   * Function bodies aren't fully supported yet
   * #defines can only be converted to FB if their bodies can be parsed as
     expression. Random/incomplete token sequences can't be translated though.

  fbfrog is able to read multiple headers or multiple versions of the same
  header (preprocessed differently) and merge them into a single binding.
  1. This is used to support multiple targets (DOS/Linux/Win32, x86/x86_64):
     Instead of looking for #ifs in the input headers and possibly trying to
     preserve those, fbfrog preprocesses and parses the input header files
     multiple times (using different predefines each time), and then merges the
     resulting target-specific APIs into one final binding, by (re-)inserting
     #ifs (such as #ifdef __FB_WIN32__) where needed.
  2. and by using the -declare* command line options yourself you can combine
     pretty much any APIs, for example version 1.0 and 2.0 of a library, or the
     ANSI and UNICODE versions of a Win32-specific header. Of course it only
     makes sense if the APIs belong together.


Compiling:
  fbc *.bas -m fbfrog

Running the tests:
  1. fbc tests/run.bas
  2. tests/run
  3. Use Git to check the status of the tests/ directory.
     Any changes indicate test failures.


Usage:

  You run fbfrog, passing the *.h file(s) that you want to translate on the
  command line.
    fbfrog foo.h

  It's only necessary to pass the "entry points" - the header(s) that you would
  #include in a C program. fbfrog will (by default) expand all #includes it can
  find. The generated binding will cover the API that would become available by
  #including those headers in the given order. Separate headers that aren't
  intended to be #included together shouldn't be passed to fbfrog together, but
  in separate invocations.

  With a little bit of luck it will be able to generate a *.bi file immediately;
  if not, there may be a parsing error reported, or a "TODO: unknown contruct"
  embedded in the *.bi file. This can be caused by various things, for example:
   * A construct that fbfrog's parser cannot handle yet. This can be solved by
      a) fixing fbfrog, so please report missing features!
      b) manually fixing up the unknown contructs in the generated .bi file
   * A #define that is too complex to be converted to FB automatically - it must
     be translated (or perhaps removed from the binding) manually.
   * An unexpanded macro, for example if the #define wasn't seen because it's
     inside an #included file that fbfrog couldn't find and thus skipped. This
     can be solved by
       a) helping fbfrog find the #include (-incdir <path>), or
       b) telling fbfrog to expand that macro (-define <identifier> [<body>])


Using the -declare*/-select/-ifdef options:

  Assuming we have the header files foo1.h and foo2.h, let's use the following
  fbfrog options:

    -declareversions __LIBFOO_VERSION 1 2
    -select __LIBFOO_VERSION
    -case 1
        foo1.h
    -case 2
        foo2.h
    -endselect

  Save those options into a foo.fbfrog helper file (because it's too much to
  type at the command line), and pass it to fbfrog:

    fbfrog @foo.fbfrog

  The created binding will allow the user to #define __LIBFOO_VERSION to 1 or
  2 in order to select that specific API version:

    [...declarations that existed in both foo1.h and foo2.h...]
    #if __LIBFOO_VERSION = 1
        [...declarations that existed only in foo1.h...]
    #else
        [...declarations that existed only in foo2.h...]
    #endif
    [...etc...]

  You can use -declare* options as wanted to support multiple APIs in 1 binding:

    -declareversions <symbol> <numbers...>
        Useful to allow selecting an API by version. This will produce #if
        blocks such as #if <symbol> = <number>.

    -declaredefines <symbol1> <symbol2> <symbol3>
        Useful to allow selectin an API by #defining a certain symbol. This is
        used in builtin.fbfrog to allow selecting an OS-specific API based on
        the __FB_DOS__/__FB_LINUX__/__FB_WIN32__ #defines, but it could be used
        for other things aswell. This will produce #if blocks such as #ifdef
        <symbol1>. The symbols are assumed to be #defined exclusively - only one
        at a time.

    -declarebool <symbol>
        Useful to allow API selection based on whether a certain symbol is
        defined or not. This is used for __FB_64BIT__ in builtin.fbfrog, but
        could also be used to support distinguishing between UNICODE and ANSI
        versions of a binding (-declarebool UNICODE -> #ifdef UNICODE) or
        the shared library/DLL version and the static library version, etc.

  If multiple -declare* options are given, they multiply. For example:
    -declarebool A -declarebool B
  produces these APIs:
         defined(A)  and      defined(B)
         defined(A)  and (not defined(B))
    (not defined(A)) and      defined(B)
    (not defined(A)) and (not defined(B))

  You can use the -select/-ifdef logic options to create different "code paths"
  where some options will only be used for some APIs (instead of applying to
  all APIs). This also works with -declare* options, allowing you to build
  even complex API condition trees. An example from builtin.fbfrog:
      -declaredefines __FB_WIN32__ __FB_LINUX__ __FB_DOS__
      -ifdef __FB_DOS__
      -else
          -declarebool __FB_64BIT__
      -endif
  This declares APIs for win32/linux/dos, and then turns those into non-64bit
  and 64bit versions - except for dos because that doesn't support 64bit.


Why use a custom C preprocessor and parser instead of an existing one?
  fbfrog wants to do things that existing preprocessors/parsers don't support:
    * preserve #defines and parse their bodies
    * selective/optional expansion of macros and #includes
    * fully configurable pre-#defines, no hard-coded target/compiler-specifics
    * modify the AST to make it FB-friendly, insert FB-specific constructs


Why make fbfrog a standalone tool? How about integrating C parsing (or rather,
a binding creation tool) into fbc itself directly, so bindings would become
unnecessary?
  1. Until we can translate all constructs to FB automatically, the .bi files
     are needed as a "buffer" where we can fix things up manually.
  2. fbc's existing lexing/preprocessing/parsing code could not be reused to
     parse C headers, because it's way too different from C. Thus it'd be
     necessary to maintain a full binding creation tool as part of fbc, and it'd
     be more or less the same amount of work as keeping it separate -- although
     having it included in fbc would probably be more convenient for the user.
  3. fbc is the FB compiler, not a C compiler, and FB itself should be good
     enough to specify the API/ABI for accessing external libraries.
  4. Users may expect to be able to interact with the #included .h file directly
     by using FB #define statements etc., but that wouldn't be possible unless
     we had a FB-to-C translator too.


To do:

- name fixup: distinguish FB core & quirk keywords, to prevent unnecessary renames for fields
- Change console output, it's too much if there are more than 10 #includes...
    * ideally no output at all
    * especially: no progress indicator, because the tool should be fast enough
      to make it unnecessary, and it's not bound by disk or network anyways.
    * However, information about #included file names is important, to see what
      got included in the binding, which files weren't found, what file names
      could be used for the pattern-based -nomerge option, etc.
    * support -o - to write to stdout
- Instead of RENAMELISTs, how about just emitting "'' RENAMED: foo => bar" notes
    * because the RENAMELISTs can become very long
    * If keeping RENAMELISTs, then merging must support them nicely
- Add pattern-based -nomerge option, or default to not preserving #included code
  use a -include <pattern>
- How to handle unknown constructs? Must be manually fixed - i.e. removing or
  replacing with nice FB code. How to automate that?
     => -removedefine
     => -setdefinebody <symbol> "<FB body text>"?
- If an #include was found but not inserted, then the #include statement should
  be preserved (but renamed .h -> .bi)
- #includes that can't be found should just be removed, e.g. system headers
- probably should move #includes to the top, out of the extern block

- Continue support for parsing function bodies: if/else blocks, for/while/do/while
  loops, local vars, assignments, goto, break, return, switch, labels including 'case'.
  - change hScopeBlockOrInitializer(): depending on context it doesn't need and
    shouldn't do this disambiguation
  - Scope block parsing should be the same as parsing function bodies
  - Identifier fixup inside scopes
  - How to handle assignments in the middle of expressions or as if/while conditions?
        if (a = 1) ...          =>    a = 1 : if a then ...
        if (a = 1 && b = 2) ... =>    a = 1 : if a then : b = 2 : if b then ... : end if
    ?: and &&/|| operands containing assignments must be expanded to real if blocks.

* Performance issues: AST walking (especially the symbol renaming) & identifier comparisons
  Could be fixed by using a symbol table, so ASTNODEs would reference symbols
  instead of storing identifiers. Then the renaming functions would simply
  rename the symbol instead of having to walk the entire AST replacing
  occurrences of the identifier, and identifier comparisons become pointer
  comparisons instead of string comparisons.
  Things like astFixArrayParams() and astTurnPlainZstringIntoByte() are very
  similar and can either be combined into 1 walking function and if not too ugly
  it can be handled during emitting instead of an extra walking function.
