
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

  Create .bi binding for a C header:       ./fbfrog foo.h
  Do it for all C headers in a directory:  ./fbfrog path/to/foo/
  ...

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

- Massively reduce default fbfrog stdout output
  - should just print the names of generated .bi files, per preset (if any)
  - ideally could show some progress indicator, not necessarily in %, but at least
    a counter like "parsing files 2/20...", or "parsing file 1/20, version 1/3, target 1/3"
  - also something for the AST merging step, which could be a huge part
    - test how long the AST merging takes for huge headers
    - could a progress indicator be shown? yes, based on decltables and number of decls in the result

- can't scan /usr/include, dir() problem?

- #include foo.h  ->  #include foo.bi, if foo.bi will be generated too
- #include stdio.h -> #include crt/stdio.bi, for some known default headers
  (or perhaps let presets do this)

- add pass to check all identifiers against FB keywords, and to check for dupdefs
  due to case insensitivity

- Add to top of binding:
    - if versiondefine not #defined, use a default version
    - complain if versiondefine #defined to unsupported value
    - for __FB_<target>__ #defines we can ensure the header is only used on
      systems that it supports

- For #defines that can't be parsed & translated as simple expressions:
  REPLACE DEFINE Symbol [MacroParams] OldBody "NewBody"
  - "NewBody" is FB code in a C string, as the preset is lexed in C mode
- Add BOOLDEFINE to mark a macro as "returns a bool", so the C #define parser
  can set is_bool_context=TRUE when folding

- lex: should only allow escaped EOLs in C mode
- lex: add support for FB escape sequences, or at least only allow C escapes
  in C mode

- -m should somehow allow concatenating even #includes with refcount >= 2.
    - if multiple leaf headers #include a common header each once, then the
      leafs should be concatendated into 1, and the common header prepended at
      its top, and the #includes for it should be removed.
    - this isn't right as it changes the order of code which could break
      declaration dependencies, but at least presets should be allowed to enable this

- Macro expansion should preserve token locations, perhaps even a stack of
  locations in case of nested macros. It'd be nice if the tkOops() functions
  could show the context of a token that caused an error as it appears in the
  tk buffer, and then where the token came from...
- Error context output should expand/align TABs to 8 spaces based on column position from BOL
  as it would happen in the source file
- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens

- Improve FB parser to allow re-importing generated bindings, this could be
  used to amend bindings much more easily than re-making from scratch (which may
  require downloading many tarballs for different versions of the library),
  and it allows modifications made to the binding to be preserved...

- libzip: zip_source_free() vs. enumconst ZIP_SOURCE_FREE,
          zip_stat_index() vs. #define ZIP_STAT_INDEX
