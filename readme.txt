
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
    fbfrog will parse input files registered for each version, do preprocessing
    with the symbols registered for each version, and combine the resulting
    declarations into a single binding. fbfrog will try to combine declarations
    common to all versions, and put other declarations (that exist in some
    versions only, but not all) inside #if checks such as
    <#if __MYLIB_VERSION__ = 1>, which allows the binding's user to select the
    API version by #defining __MYLIB_VERSION__ as wanted.

To do:

- Re-add "unknown construct -> TODO" handling
    + inline functions etc. are likely too hard to translate properly automatically
    + having 99% good result with some declarations commented out is a better user
      experience than no result at all
    * Example:
         FOO void f(void);
      Produces:
         '' TODO: fbfrog: foo.h(123): unknown construct; declaration parser expected identifier but found "void"
         'FOO void f(void);
         ''   ^~~~

	* need ASTCLASS_UNKNOWN which holds the series of tokens for that construct
	  so that it can be written into the output file
	    + they're at the same position they would be if translation would have worked
	    + they automatically benefit from version-specific handling/AST version merging
	    + much better to put them in-line than to report them on stdout
	* C parser needs to do trial and error parsing again
	   * set context (struct/union/enum, typedef, declaration, expression)
	   * we can decide very early & easily which context of these it is
	   * remember 1st token position (begin of construct)
	   * if doesn't match any construct, or parsing of selected construct fails,
	     go back to 1st token and try to find end of construct, and turn that into UNKNOWN node.
	   * all parsing functions need to return TRUE/FALSE
	   * token locations become unnecessary? only needed for error reports, but if we keep
	     the tokens as seen by the C parser in the UNKNOWNs, then that's even better than
	     the original location. It's not necessary useful to report the original source location (filename/linenum)
	     although that would be nice too.

- Need some way to easily pass tk locations on to AST nodes, perhaps astNewFromTK()
  or similar that copies over the location automatically
- Need some high-level way to combine locations into one, etc. which is currently
  done manually in hParseArgs() at least
- Add astFilter() to read in a GROUP and produce a new GROUP filtered by 1 or 2 classes (but cloned children of course)?
- use CONSTI and CONSTF nodes instead of just CONST, so we don't need typeIsFloat() checks?
- Remove hShell(), hMkdir[P], hKill...
- Try using ASTNODEs instead of DIRNODE/DIRQUEUE

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
- Macro expansion should preserve token locations, perhaps even a stack of
  locations in case of nested macros. It'd be nice if the tkOops() functions
  could show the context of a token that caused an error as it appears in the
  tk buffer, and then where the token came from... an even better, show the line
  of code in each state from current (as seen by the parser) to original.
- when seeing #error, report "found an #error" instead of the #error's message,
  otherwise it looks like that message is coming from fbfrog
- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens
