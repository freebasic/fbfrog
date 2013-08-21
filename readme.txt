
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

- Add some kind of automated formatting. fbfrog should be able to optionally
  preserve comments and dividers, because that's useful for some headers, but
  otherwise don't bother.

- Add output directory option

- libzip: zip_source_free() vs. enumconst ZIP_SOURCE_FREE,
          zip_stat_index() vs. #define ZIP_STAT_INDEX

- Allow to presets to download and extract tarballs, and then register some of
  the extracted files as input files for parsing

- Improve FB parser to allow re-importing generated bindings, this could be
  used to amend bindings much more easily than re-making from scratch (which may
  require downloading many tarballs for different versions of the library),
  and it allows modifications made to the binding to be preserved...

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

- ## PP merging should allow TK_ID ## TK_DECNUM or TK_UNDERSCORE,
  currently it only allows TK_ID ## TK_ID

- add pass to check all identifiers against FB keywords, and to check for dupdefs
  due to case insensitivity

- must check for #if/#endif blocks split up by VERSIONs: the #if and #endif directives
  could end up in separate VERSIONs currently, that wouldn't be good if the VERSIONs
  themselves are implemented as #if/#endif...
    - perhaps #if/#endif can be merged into #if blocks so they appear as single nodes
      that will be merged or not, like structs and their fields?
- VERSION nodes should be merged/turned into #if checks on the binding's version #define

- astVersionsMatch() should allow matches also if version numbers are in different order,
  though currently that won't ever happen since versions are always merged in the same order

- Perhaps load presets from .ini files afterall, one .ini per preset or similar
    - Working on/adding/sharing presets becomes easier because fbfrog itself
      doesn't need to be patched, rebuilt, re-released, etc.
    - Things like the list of known symbols are better stored in a config file
      than in form of a bunch of hard-coded ppAddSym() calls, because the config
      file allows for a smaller and potentially easier to read format.
    - Of course no "dynamic" coding will be possible, so everything must be
      encoded in the format: tarball URLs, input file names, version numbers,
      restricting certain symbols to certain versions, etc.
    - Allow passing multiple .ini's on command line to run multiple presets one
      after another (can even use fbfrog sub-processes if needed), this would
      allow for .ini's for more complex test cases, to be run simply via
          fbfrog tests/*.ini
