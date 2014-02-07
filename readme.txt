
  fbfrog -- FreeBASIC binding generator
  Copyright (C) 2011 - 2013  Daniel C. Klauer <daniel.c.klauer[at]web.de>

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


Goal:

  Automation of most of the work needed to create and maintain FB bindings to
  C libraries, most importantly: converting C declarations to FB code.


Features:

  * C pre-processor (CPP)
      * tracks #defines/#undefs and does macro expansion (can be disabled for
        specified symbol)
      * preserves #define statements (can be disabled for specified symbol)
      * expands #includes (always, so it gets to see #defines/#undefs, but the
        #included code can be removed before it's given to the C parser)
      * Evaluates #if blocks (always), based on an #if expression parser
      * Allows for pre-#defines etc. to be specified

  * C parser
      * Recursive declaration parser that can handle multiple declarations in
        the same statement, nested declarations, including function pointers
        returning function pointers etc. Also parses some __attribute__'s, e.g.
        calling conventions.
      * Expression parser that's used to parse initializers, enum constant
        values, #define bodies.
      * Parses typedefs, structs, unions, enums, including nested
        structs/unions.

  * FB binding creation
      * C expressions are converted to FB ones and then simplified to become
        pretty without changing values.
      * Array parameters are turned into pointers.
      * Anonymous structs are given the name of the (first) typedef that uses
        them, and the typedef is removed.
      * Redundant typedefs (<typedef struct A A;>) are removed as in FB there
        are no separate type/tag namespaces. <struct A> or <A> translate to the
        same thing.
      * Conflicting identifiers (conflicts with FB keywords, or amongst the
        symbols declared in the binding, possibly due to FB's case
        insensitivity) are fixed by appending _ underscores to the less
        important symbol (e.g. renaming #defines if preferred over renaming
        procedures). For renamed variables/procedures, ALIAS "<original-name>"
        will be emitted.
      * Extern blocks are added around the binding's declarations, for the
        calling convention that the binding uses most often. Calling conventions
        and case-preserving ALIASes are only emitted on procedures if they're
        not covered by the Extern block (happens if a binding's procedures use
        multiple calling conventions).
      * Multiple parsing passes (each representing the binding for a certain
        target system as in win32 or linux etc., or for a certain version of
        input headers as in library 1.0 and library 2.0, etc.) produce multiple
        ASTs per input header. These version/target-specific ASTs are merged
        into one to produce the final binding. Declarations common to all
        versions/targets are inserted once into the final binding. Other
        declarations (that exist in some versions only, but not all) are put
        inside #if blocks (e.g. <#if __MYLIB_VERSION__ = 1>, which allows the
        binding's user to select the API version by #defining __MYLIB_VERSION__
        as wanted. Target-specific declarations are put into #if blocks checking
        FB's target-specific compiler-#defines (e.g. <#ifdef __FB_WIN32__>).


Compiling:

  fbc *.bas -m fbfrog


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

- Need some way to easily pass tk locations on to AST nodes, perhaps astNewFromTK()
  or similar that copies over the location automatically
- Need some high-level way to combine locations into one, etc. which is currently
  done manually in hParseArgs() at least
- Add astFilter() to read in a GROUP and produce a new GROUP filtered by 1 or 2 classes (but cloned children of course)?
- use CONSTI and CONSTF nodes instead of just CONST, so we don't need typeIsFloat() checks?
- Remove hShell(), hMkdir[P], hKill...
- Since the C parser no longer modifies the tk buffer, could it be turned into
  an array? No, currently it still temporarily re-inserts macro bodies, but is
  that really needed?
- Remove all remaining cases of tkInsert() without corresponding tkSetLocation(),
  e.g. ## token merging, so tkReport() can be simplified. (although, tkToCText()
  etc. would still be useful for showing macro expansion stack history)
- Error messages should show the macro expansion steps that happened in a given
  piece of code, from what the parser saw to the main location where it all came
  from.

- report "(N declarations and M errors)" for each emitted file
- astMakeProcsDefaultToCdecl() should be unnecessary, it should be done by the
  C parser already
- Consider adding symbol tables separate from the AST, so e.g. all UDT dtypes
  would reference the same subtype symbol instead of allocating an id everytime.
    - define/const/proc/var ids aren't re-used much, but type ids are. Perhaps
      add a UDT map? ie. use dtype values >= TYPE_UDT to represent UDTs (by name)
      Then UDT types wouldn't have to allocate a subtype ID anymore (only function pointers).
    - of course having the completely self-contained AST is great aswell
- Do not preserve #defines that are #undeffed, such that ultimately it'll become
  pointless to preserve #undefs at all
- Support 32bit vs 64bit somehow? C long is already mapped to CLONG, but what
  if a header checks for 32bit/64bit pre-#defines? Just add linux64/win64/etc. os blocks?
- Long Double and other built-in types that FB doesn't have:
    a) just omit, except fields in a struct that is needed
    b) replace with byte array, other dtypes, or custom struct
- Prettier "assuming undefined" reports: just 1 line, no source context
- Only one "assuming undefined" report per symbol (at least only one per CPP run,
  perhaps even only one per fbfrog run)
- emit case-preserving ALIASes if outside Extern block (i.e. no HIDDENCALLCONV flag)
- Add option to translate #defines to consts if possible
- #include foo.h  ->  #include foo.bi, if foo.bi will be generated too
- #include stdio.h -> #include crt/stdio.bi, for some known default headers
  (or perhaps let presets do this)
- CPP works too much like FB's PP (e.g. recursive expansion...)
- Check params/fields for name conflicts
- Emit list of renamed symbols at top of header
- Add BOOLDEFINE to mark a macro as "returns a bool", so the C #define parser
  can set is_bool_context=TRUE when folding
- parentheses around macro params should be preserved (can use a flag on the AST node)
- Enums: must be emitted as Long for 64bit compat:
	Type Foo As Long (and make enum anonymous)
	Enum Foo As Long (must be added to FB first)
- Support bitfields?
- when seeing #error, report "found an #error" instead of the #error's message,
  otherwise it looks like that message is coming from fbfrog
- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens
