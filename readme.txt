
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

- CPP errors when re#defining C keywords because it only accepts TK_ID as #define id

- Should remove *.fbfrog files and use @response files and command line options
  for everything

- use CONSTI and CONSTF nodes instead of just CONST, so we don't need typeIsFloat() checks?
- should re-add support for unknown-construct-error-recovery (emitting TODOs+original code in comment)
    - C constructs, and for CPP, #pragmas and such

- #include foo.h  ->  #include foo.bi, if foo.bi will be generated too
- #include stdio.h -> #include crt/stdio.bi, for some known default headers
  (or perhaps let presets do this)

- add pass to check all identifiers against FB keywords, and to check for dupdefs
  due to case insensitivity

- For #defines that can't be parsed & translated as simple expressions:
  REPLACE DEFINE Symbol [MacroParams] OldBody "NewBody"
  - "NewBody" is FB code in a C string, as the preset is lexed in C mode
- Add BOOLDEFINE to mark a macro as "returns a bool", so the C #define parser
  can set is_bool_context=TRUE when folding

- parentheses around macro params should be preserved (can use a flag on the AST node)

- how to handle Enums? Need to be translated to LONG currently, but it's hard to
  detect what's an enum and what isn't...
    - look for enums in the API
    - look for data types like "enum foo"...
    - or just add "enum FOO as long" to FB and translate all enums to that
    - of course that can't be done for enums that aren't declared in this API,
      but for example in system headers, but those should be translated properly
      in our crt/ bindings etc.

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

- libzip: zip_source_free() vs. enumconst ZIP_SOURCE_FREE,
          zip_stat_index() vs. #define ZIP_STAT_INDEX
