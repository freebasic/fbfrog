
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


Usage:

  Pass *.h files (C API declarations) to fbfrog:
    $ ./fbfrog foo.h
  and fbfrog generates a corresponding *.bi file (FB API declarations).

Compiling:

  fbfrog:
        fbc -m fbfrog *.bas

  Linux GTK+ GUI:
        cd gui/
        fbc gui.bas gtk.bas xpms.bas -mt -x ../fbfrog-gui

  Win32 GUI:
        cd gui/
        fbc gui.bas win32.bas resources.rc -mt -s gui -x ../fbfrog-gui.exe

  The *.png images were hand-crafted in Paint.NET.
  The win32 icon can be rebuild using GIMP: Open the 48x48 png, then load in
  the others as layers, then duplicate them to have three of each, then save
  as .ico and set the options so that it ends up with 8bpp, 24bpp and 32bpp
  versions of each image.
  The *.xpm files are exported from the *.pngs using GIMP.
  To recreate the xpms.bas module, compile and run embed-xpms.bas.

To do:

- @response files instead of *.fbfrog files
	+ command line = preset, same syntax, same parser, good for serious testing,
	  good for sharing, good for quick copy/paste tests...
	- potentially slightly less pretty syntax
- use CONSTI and CONSTF nodes instead of just CONST, so we don't need typeIsFloat() checks?
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
- Translate #defines to consts if possible (it would be the more proper FB translation...)
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
