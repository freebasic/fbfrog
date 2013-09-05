
  fbfrog -- FreeBASIC binding creation tool
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

  Translate a header:                 ./fbfrog foo.h
  Translate a whole directory:        ./fbfrog path/to/foo/
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

- How to translate #define bodies?
  - Most #defines are just constants (expressions) and can be parsed & translated
  - The rest is partial C constructs or relies on macro expansion etc. They can
    only be removed, and if wanted an FB translation can be given manually after
    the PP pass.
        REPLACE DEFINE OldCBodyTokens "new FB body text"
        - having the old C body tokens allows us to compare and error if the
          macro changed, in which case the preset must be adjusted
    - REMOVE DEFINE Identifier works at the same level now, could perhaps be
      integrated with this

- version specific parts of preset:
    - should use VERSION blocks in a GROUP to represent
    - anything at toplevel will be under VERSION block covering all versions
    - this requires all DECLARE VERSION statements to be at the top
    - then perhaps we can use the same solve-version-out code to retrieve the
      preset statements for a specific version?
- since LEXMODE_FBFROG now uses all keywords, "undef string" would appear
  as two keywords instead of keyword+id and fail to be parsed. Need to find
  better solution than sharing keywords between C/FB/fbfrog or allow
  anything >= TK_ID as arguments to undef|define|expand|macro.
  Some bindings may need to register symbols that happen to be FB keywords etc...

- Macro expansion should preserve token locations, perhaps even a stack of
  locations in case of nested macros. It'd be nice if the tkOops() functions
  could show the context of a token that caused an error as it appears in the
  tk buffer, and then where the token came from...
- Error context output should expand/align TABs to 8 spaces based on column position from BOL
  as it would happen in the source file
- Stack of error context information, hints: for example the parser could
  errPushHint( ERRHINT_* ) before parsing an expression, so if there is an error
  while parsing the expression, the error report could contain the information
  what the expression is for.

- Improve FB parser to allow re-importing generated bindings, this could be
  used to amend bindings much more easily than re-making from scratch (which may
  require downloading many tarballs for different versions of the library),
  and it allows modifications made to the binding to be preserved...

- Add output directory option

- libzip: zip_source_free() vs. enumconst ZIP_SOURCE_FREE,
          zip_stat_index() vs. #define ZIP_STAT_INDEX

- CPP relational BOPs shouldn't just be emitted as FB relational BOPs always,
  because the result values differ (1 vs. -1). This should only be done where
  a boolean check is done, but not if it's used in a math expression.
  -> related to ! vs. not: "!defined" should be turned into "not defined" if
     used in boolean context.
  -> is_bool parameter to hFold(), which can be set to TRUE by callers for
     iif/#if condition for example.
  -> it's not clear whether AST should represent C or FB?
     -> maybe have both C/FB relational/defined ops

- #include foo.h  ->  #include foo.bi, if foo.bi will be generated too
- #include stdio.h -> #include crt/stdio.bi, for some known default headers
  (or perhaps let presets do this)

- add pass to check all identifiers against FB keywords, and to check for dupdefs
  due to case insensitivity

- astVersionsMatch() should allow matches also if version numbers are in different order,
  though currently that won't ever happen since versions are always merged in the same order

- wchar_t not translated to wstring?

- nested #includes should be frogAddFile()'d in their parent include file context,
  not in the toplevel file context, shouldn't they?

- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens
