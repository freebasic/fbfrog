
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


Features:

  This program reads in *.h files (C API declarations) and generates
  corresponding *.bi files (FreeBASIC API declarations). It automates most of
  the work needed to create and maintain FB bindings to C libraries, most
  importantly: converting C declarations to FB code.

  * C pre-processor (CPP)
      * tracks #defines/#undefs and does macro expansion (can be disabled for
        individual symbols)
      * preserves #define statements (can be disabled for individual symbols)
      * expands #includes (always, so it gets to see #defines/#undefs, but the
        #included code can be removed before it's given to the C parser)
      * evaluates #if blocks (always), based on an #if expression parser
      * allows for pre-#defines etc. to be specified

  * C parser
      * Recursive declaration parser that can handle multiple declarations in
        the same statement, nested declarations, including function pointers
        returning function pointers etc. Also parses some __attribute__'s, e.g.
        calling conventions.
      * Expression parser that's used to parse initializers, enum constant
        values, #define bodies. Includes parsing for type casting expressions,
        based on a heuristic (whether the identifier in parentheses looks like a
        type or is a known typedef, etc.).
      * Parses typedefs, structs, unions, enums, including nested
        structs/unions.

  * FB binding creation
      * C's built-in data types (using the sizes typically used for x86) and
        also typedefs such as size_t or int32_t are directly converted to
        corresponding FB data types. <signed|unsigned char> is translated to
        BYTE|UBYTE, while plain <char> is assumed to mean ZSTRING.
      * C expressions are converted to FB ones, as pretty as possible without
        changing result values.
      * Array parameters are turned into pointers.
      * Anonymous structs are given the name of the (first) typedef that uses
        them, and the typedef is removed.
      * Redundant typedefs (<typedef struct A A;>) are removed, because in FB
        there are no separate type/tag namespaces. <struct A> or <A> translate
        to the same thing.
      * #defines "nested" inside struct bodies are moved to the toplevel. Even
        though FB doesn't scope #defines inside UDTs (only inside scope blocks,
        but not namespaces), this is better, because it represents the original
        header's intentions more closely. It becomes more important when
        converting #defines to constant declarations, because FB does scope
        those inside UDTs.
      * Conflicting identifiers (conflicts with FB keywords, or amongst the
        symbols declared in the binding, possibly due to FB's case
        insensitivity) are fixed by appending _ underscores to the less
        important symbol (e.g. renaming #defines is preferred over renaming
        procedures). For renamed variables/procedures, ALIAS "<original-name>"
        will be emitted.
      * Extern blocks are added around the binding's declarations, for the
        calling convention that the binding uses most often. Calling conventions
        and case-preserving ALIASes are only emitted on procedures if they're
        not covered by the Extern block (happens if a binding's procedures use
        multiple calling conventions).
      * Multiple parsing passes produce multiple ASTs, each representing the
        binding for a certain target system, e.g. win32 and linux, or for a
        certain version of input headers, e.g. v1.0 and v2.0. These ASTs are
        then merged into one to produce the final binding. Declarations that
        exist in one version only, or can be used on a certain target only, are
        put inside #if blocks, e.g. <#if __MYLIB_VERSION__ = 1> or
        <#ifdef __FB_WIN32__>. To select the API for a specific version, the
        binding's user can #define __MYLIB_VERSION__ as wanted.


Compiling:

  $ fbc *.bas -m fbfrog


Running the tests:

  1. $ fbc tests/run.bas
  2. $ tests/run
  3. Use Git to check the status of the tests/ directory. Any changes indicate
     test failures.


Usage:

  Pass *.h files (C API declarations) to fbfrog:
    $ ./fbfrog foo.h
  and fbfrog generates a corresponding *.bi file (FB API declarations).

  Creating a binding for multiple versions of a library:
    $ ./fbfrog -version 1.0 foo1.0.h -version 2.0 foo2.0.h

  Creating a binding for GCC 4 Win32/Linux versions of a header:
    options.txt:
        -define __GNUC__ 4
        -target win32
            -define _WIN32
        -target linux
            -define __linux__
    $ ./fbfrog foo.h @options.txt

  -version can be used to tell fbfrog about multiple versions of a binding.
  -target is similar and causes fbfrog to parse multiple versions of the
  binding as they would be used when compiling for the respective systems.
  -targets can be nested in -versions. fbfrog will parse input files registered
  for each version/target, do preprocessing with the symbols registered for each
  version/target, and combine the resulting declarations into a single binding.


To do:

- CPP works too much like FB's PP (e.g. recursive expansion...)
    - macro should be disabled for expansion while its body is parsed,
      but not while parsing its args (if it's a function-like macro)
    - at end of macro body, the macro should be re-enabled for expansion
    - macro args must be fully macro-expanded before being inserted into
      params in the macro body, unless they're used with # or ## sometimes be fully macro expanded before inserted in
      place of the param, depending on how the param is used.
    - ## merging doesn't handle float literals, e.g. 1##. or .##0 ?!

- #define handling
    - Do not preserve #defines that are #undeffed, such that ultimately it'll
      become pointless to preserve #undefs at all
    - #defines that can be determined to be constants should automatically be
      emitted as CONSTs

- 64bit support:
    * C long is already mapped to CLONG
    * pre-#defines are missing
    * need to use x86 32bit/64bit compiler #defines
    * Use -target dos,linux,x86_64-linux,win32,x86_64-win32? or something else?
    * Why bother having -target at all. Could just always parse headers for all
      available modes... (perhaps allow disabling some modes if that's really
      needed, e.g. if the header contains an #error for a certain target)
      In general the headers work for all systems even if the library isn't
      compilable for all targets.
    * What target-specifics do we really have to handle? It's usually just that
      headers #include certain target-specific headers or different declarations.
- Enums: must be emitted as Long for 64bit compat:
	Type Foo As Long (and make enum anonymous)
	Enum Foo As Long (must be added to FB first)
- Long Double and other built-in types that FB doesn't have:
    a) just omit, except fields in a struct that is needed
    b) replace with byte array, other dtypes, or custom struct

- #include foo.h  ->  #include foo.bi, if foo.bi will be generated too
- #include stdio.h -> #include crt/stdio.bi, for some known default headers
  (or perhaps let presets do this)

- Emit list of renamed symbols at top of header

- Add -booldefine to mark a macro as "returns a bool", so the C #define parser
  can set is_bool_context=TRUE when folding

- parentheses around macro params should be preserved (can use a flag on the AST node)
- Are forward declarations/references handled correctly? Consider merging the
  {STRUCT|UNION|ENUM}FWD into one since in FB they'd all be emitted as the same
  code anyways?!
- Support bitfields?
- Const folding etc. has issues with 32bit/64bit, using Longint internally, so
  e.g. ~(0xFFFFFFFF) comes out as &hFFFFFFFF00000000 instead of &h0. Need to
  respect C number literal dtypes & sizes.
- Don't crash on (INT_MIN / -1) or (INT_MIN % -1)
- Const folding probably also doesn't handle unsigned relational BOPs properly
  since it only does signed ones internally
- Remove stats stuff and do real profiling with huge headers

- Should use FILE nodes to represent C parser result, and have file*() functions
  that work on that AST, and extract them from ast.bas into separate modules.
  AST merging should merge "files" instead of "ASTs"...
- The filebufferFromZstring() function also re-uses FILEBUFFERs based on the id,
  so fbfrog needs to ensure to use proper unique ids, or else the same FILEBUFFER
  could be reused for different zstrings. For filebufferFromFile() we assume this
  to be the wanted behaviour because the filename is unique, but internal strings are a different story...
- Pre-#defines, -removematch, etc. text is loaded into tk using filebufferFromZstring/lexLoadC,
  this process loses the command line location info. Should allow passing a source/base/start
  location to filebufferFromZstring and lexLoadC so that it can reports lexing errors with the
  command line context, instead of temp strings that are used to feed filebufferFromZstring.
  To do this, each pre-#define/#include etc. should also be passed to filebufferFromZstring/lexLoadC
  separately. filebufferFromFile() already accepts a srcloc for similar reasons: to be able to
  report the "file not found" in the proper context. Of course filebufferFromFile() doesn't need
  to pass it on to lexLoadC() because any issues with the loaded file should be reported in the context
  of that file, not the code that caused that file to be #included. It makes sense though for "inline code",
  such as pre-#defines and -removematch...
- Need some high-level way to combine locations into one, etc. which is currently
  done manually in hParseArgs() at least
- Need better error messages for things like "missing ';' to finish declaration"
  should perhaps underline the entire declaration?
- TK_EOF should have location information (toplevel file?), but then we need
  TK_BOF too or else the two could be confused. Reporting errors like missing
  #endif at last token instead of EOF feels wrong... also, cppMain() removes
  #ifs from the tk buffer, so tkReport() can't rely on tk buffer content in that
  case, and eof-1 may point at an unexpected token...
- Remove all remaining cases of tkInsert() without corresponding tkSetLocation(),
  e.g. ## token merging, so tkReport() can be simplified. (although, tkToCText()
  etc. would still be useful for showing macro expansion stack history)
- Since the C parser no longer modifies the tk buffer, could it be turned into
  an array? No, currently it still temporarily re-inserts macro bodies, but is
  that really needed? Macro body parsing could surely be done after cFile():
    - cpp creates TK_PPDEFINE at proper locations (containing the AST with the macro body tokens)
    - c inserts PPDEFINE nodes into the AST at the proper locations
    - then afterwards, we go through the PPDEFINE nodes, re-insert & try to parse each body.
  Then c could use simple offsets to access tokens (tkLock/Unlock?)
  Maybe even cpp could create new token runs instead of modifying the current one,
  so that fixed TOKENRUNs could be re-used to store macro bodies, and to implement the
  macro expansion error reporting stuff...
    a) keep original tokens and a tree of expanded tokens for on each. tkGet() etc. have to
       walk the whole thing each time to find the real tokens corresponding to x. Very slow...
    b) keep expanded tokens, and parent context tokenrun on each. (context where the expansion
       happened. perhaps only 1 line, otherwise it could become too much. That's enough for error
       reporting)
- Consider adding symbol tables separate from the AST, so e.g. all UDT dtypes
  would reference the same subtype symbol instead of allocating an id everytime.
    - define/const/proc/var ids aren't re-used much, but type ids are. Perhaps
      add a UDT map? ie. use dtype values >= TYPE_UDT to represent UDTs (by name)
      Then UDT types wouldn't have to allocate a subtype ID anymore (only function pointers).
    - of course having the completely self-contained AST is great aswell
- Prettier "assuming undefined" reports: just 1 line, no source context
- Only one "assuming undefined" report per symbol (at least only one per CPP run,
  perhaps even only one per fbfrog run)
- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens

- Various tests expose weird stuff:
	triggers wrong error:
		errors/cpp/expand/merge-token-from-macro-arg-doesnt-merge.h
		errors/cpp/expand/call-zero-args-missing-parentheses.h
		errors/c/*-in-macro.h
	errors/cpp/expansion/pp-macrocall-1.h, shouldn't expand here
	errors/lex/open-*, missing source context in error
	errors/cpp/define-conflicting-duplicate, should cause error?
	errors/c/typedef-{proc|array}, should be fixed up automatically where possible and be removed otherwise
		(e.g. if the proc typedef is always used in a pointer context, then include the pointer in the typedef)
	errors/cpp/stack/*, missing #endifs reported with wrong source context
	errors/c/struct-nested-named.h, bad code-as-seen-by-fbfrog
	errors/cpp/*, bad code-as-seen-by-fbfrog?
- Use *.h.fail or similar for error tests, instead of scanning for *.h, so they
  could be put into the same directories as the normal tests?

- Show suggestions how to fix errors, e.g. if #define body couldn't be parsed,
  suggest using -removedefine to exclude the #define from the binding...
