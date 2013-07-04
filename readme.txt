
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
  C libraries, most importantly: the translation of C procedure and structure
  declarations to FB.

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

- Add output directory option
- should use TYPE_INT32 instead of TYPE_LONG, that's much less confusing
  (which "long" is it referring to, C's or FB's..)
- treat size_t etc. as built-in types
- Turn array parameters into pointers (that's the only possible translation)
- vardecl/param initializers
- drop redundant typedefs (since FB doesn't have separate struct/type namespaces)
	/* typedef-same-id-as-type fixups: */
	typedef struct T T;
	typedef struct T A, T;
	typedef struct T T, B;
	typedef struct T A, T, B;
