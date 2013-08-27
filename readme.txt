
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

- hAddTextToken() could accidentially look up comment/string bodies in the
  keyword table, then produce the wrong token?

- hRemoveOuterDIVIDERs() crashes if there's just one DIVIDER in the AST
  (.h file with just a newline)

- Must do automated formatting of macro bodies in emitter now that TK_SPACE is gone
- Macro bodies should be emitted as FB text, not C text

- Add preset files, *.txt with FB-like syntax
    - Working on/adding/sharing presets becomes easier because fbfrog itself
      doesn't need to be patched, rebuilt, re-released, etc.
    - Things like the list of known symbols are better stored in a config file
      than in form of a bunch of hard-coded ppAddSym() calls, because the config
      file allows for a smaller and potentially easier to read format.
    - Of course no "dynamic" coding will be possible, so everything must be
      encoded in the format: tarball URLs, input file names, version numbers,
      restricting certain symbols to certain versions, etc.
    - Allow passing multiple preset files on command line to run multiple
      presets one after another (can even use fbfrog sub-processes if needed),
      this would allow for presets for more complex test cases, to be run simply
      via
          fbfrog tests/*.txt
    - #defines in an FB-like preset file format could be read in with lex/tk,
      this is much nicer than hard-coding a ppMacro*() call for every single
      token of the macro body

	example.txt:
		'' Tell fbfrog about library versions, plus the version selection
		'' #define and possible values that can be used to select the
		'' API version in the final FB binding
		declare version 1.0 __MYLIB_VER__=10
		declare version 2.0 __MYLIB_VER__=20

		'' Tell fbfrog that all of the currently known versions should
		'' recieve win32/linux sub-versions, "accessible" through
		'' __FB_WIN32/LINUX__ #defines, no values given means it should
		'' use #ifdef to check for those.
		declare version *.win32 __FB_WIN32__
		declare version *.linux __FB_LINUX__

		'' Register some known symbols
		#undef _MSC_VER     '' never bother with MSVC code
		#define __GNUC__ 4  '' pretend to be gcc 4

		'' Register some known symbols only for the version 1.0 parsing passes (linux/win32):
		version 1.0
			#define ENABLE_FOO_SUPPORT
			'' Some only for version 1.0's linux parsing pass:
			version linux
				#define ENABLE_BAR_SUPPORT
			end version
		end version

		'' Can have more complex version specifiers:
		version >= 1.0, <> 4.1, *.win32
			'' Stuff only for versions >= 1.0 except for 4.1,
			'' and of those only the win32 sub-versions
		end version

		'' Share code for common symbols such as compiler/OS pre-#defines
		#include "common-symbols.txt"

	common-symbols.txt:
		'' #define _WIN32 for all win32 versions, and #undef it for all linux ones...
		version *.win32
			#define _WIN32
		end version
		version *.linux
			#undef _WIN32
		end version

	Additional commands needed:
		- download & extract & auto-cleanup tarballs: URL, filename, extraction directory name
		- register input files, relative file name (possibly from extracted tarballs)
		- ... probably many more eventually, depending on what the bindings will need

- Macro expansion should preserve token locations, perhaps even a stack of
  locations in case of nested macros. It'd be nice if the tkOops() functions
  could show the context of a token that caused an error as it appears in the
  tk buffer, and then where the token came from...

- Error context output should expand/align TABs to 8 spaces based on column position from BOL
  as it would happen in the source file

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

- ## PP merging should allow TK_ID ## TK_DECNUM or TK_UNDERSCORE,
  currently it only allows TK_ID ## TK_ID
	careful:
		#define m(a) a##0x
		void m(test)(void);
	should become:
		void m0x(void);
	and not one of:
		void m0x0(void);
		void m0(void);

- AST merging should also handle nested nodes in unions/enums, not just structs

- add pass to check all identifiers against FB keywords, and to check for dupdefs
  due to case insensitivity

- astVersionsMatch() should allow matches also if version numbers are in different order,
  though currently that won't ever happen since versions are always merged in the same order

- wchar_t not translated to wstring?

- nested #includes should be frogAddFile()'d in their parent include file context,
  not in the toplevel file context, shouldn't they?

- It'd be nice if fbfrog could preserve comments and dividers optionally
    - rely on ppComments() to distribute comments to non-whitespace tokens
    - comment given to a TK_ID that is a macro call and will be expanded should
      be given to first non-whitespace token from the expansion
    - comments from macro call argument list should just be ignored
    - comments behind #define bodies should go to the #define not the body tokens
    - comments in #define bodies should be ignored?

- should share C operator precedence table between C/PP modules
- let hFold() solve out iif( x, true, true ) and iif( x, false, false )
