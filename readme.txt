
  fbfrog -- C-to-FreeBASIC header translator
  Copyright (C) 2011 - 2012  Daniel C. Klauer <daniel.c.klauer[at]web.de>

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

  To have a .h to .bi header translation tool that can create and update FB
  headers for third-party APIs. It should produce bindings rather than exact
  translations, which are not possible anyways in most cases.

Usage:

  Translate a header:                 ./fbfrog foo.h
  Translate a whole directory:        ./fbfrog path/to/foo/
  Recursively process #includes too:  ./fbfrog --follow start.h
  Merge headers as much as possible:  ./fbfrog --follow --merge --concat *.h
  See also:                           ./fbfrog --help

  Or launch the fbfrog-gui[.exe] and use that; it's a frontend for
  the fbfrog command line program.


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

- BOL checks use -1 instead skipRev(), so won't work if there is a TK_SPACE etc.
- #defines don't emit comments yet
- how to handle number literals properly?
    - AST should store them as longints/doubles, plus dtype
    - The lexer needs to parse 'ull' type suffixes etc., so it's the one that
      decides the dtype
- should use TYPE_INT32 instead of TYPE_LONG, that's much less confusing
  (which "long" is it referring to, C's or FB's..)
- why bother with TK_DIVIDER? We want automated formatting anyways, don't need
  dividers for that...

- Combine -follow/-merge/-concat into just -merge
  - Make -merge the default
  - Remove following, external #includes will be translated separately anyways,
    or they're replaced/removed
  - For the command line UI it's best to only have 1 choice to make:
      a) 1:1 translation
      b) all:1 (merge) translation
  - Any other special cases should be handled manually by the presets

- FB needs bindings, no 1:1 translations, so use some kind of AST
    - merge low-level tokens into high-level tokens
    - don't bother preserving white-space/formatting

- Add "presets", custom hard-coded header-specific translation helpers for
  fixups before and after normal translation (e.g. renaming symbols)

- Allow registering type mappings dynamically, with built-in types added by default
  so we could remap custom types such as myint32 -> long (if wanted)

- Add #if evaluation function, to solve out useless #if blocks (that's pretty
  common for C headers which support tons of different C compilers/systems,
  while for FB only GNU C + certain targets are interesting)
- Add function to expand certain macros
- Allow presets to specify information on #defined symbols
    - assume defined to value
    - assume defined
    - assume undefined
- Auto-register new defines, based on found #defines, possibly dependant on
  #if blocks

- Comments should be associated with high level constructs, or blocks of them
  comment at EOL but behind code -> belongs to that code
  comment alone in line above line of code -> belongs to the following code
  otherwise, it's a "section divider" comment

- Add option to display #include dependency graph for the input files
  - useful to decide whether to -merge or not, to see how many "root" includes
    there are, etc.

- Add output directory option

- normalize file names in fs module, for prettier verbose output etc.

- arrays
- vardecl/param initializers
- Forward declarations
	// type T as T_
	// (this way we only need to translate the <struct T ...> body as
	// <type T_ ...>, that's easier than inserting T_ fwdref in place of T
	// everywhere where it's used before the T body)
	struct T;
- drop redundant typedefs (since FB doesn't have separate struct/type namespaces)
	/* typedef-same-id-as-type fixups: */
	typedef struct T T;
	typedef struct T A, T;
	typedef struct T T, B;
	typedef struct T A, T, B;
