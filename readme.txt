
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

- Consider stopping to bother with preserving commentary, for a pure binding
  with auto-formatting it's not needed
- Add output directory option
- libzip: zip_source_free() vs. enumconst ZIP_SOURCE_FREE,
          zip_stat_index() vs. #define ZIP_STAT_INDEX
- Allow to presets to download and extract tarballs, and then register some of
  the extracted files as input files for parsing
- If any files are given on command line, that overrides the preset (if any)
- Support parsing separate library-version-specific files and then combining
  their ASTs with a simple diff algorithm to create a binding that supports
  multiple versions of that library through a version #define
- Add AST exporting & importing support, so the ASTs of finished bindings could
  be stored in fbfrog.git, allowing these bindings to easily be extended when
  new versions of their libraries come out (so fbfrog wouldn't have to re-parse
  or re-download all the tarballs for previous versions again)
    - Ideally the final FB binding itself could be re-imported but that'll be
      more difficult

- should be able to solve the calling conventions problem automatically,
  because otherwise a human has to check every single function declaration,
  or make unsafe assumptions...
	#ifdef _WIN32
		#define DLL_CALLCONV __stdcall
	#else
		#define DLL_CALLCONV
	#endif
	void DLL_CALLCONV FreeImage_DeInitialise(void);
  - Preset tells us that DLL_CALLCONV needs to be expanded
  - We look for DLL_CALLCONV declaration
  - And find two, both of which depend on certain #ifs, since they're nested
    in #ifs
  - So now we need to parse the (rest of the) header twice, once assuming the
    #if code path with the one declaration, once for the other
  - This could be done duplicating the token buffer and removing the other
    unreachable code paths
  - Then the remaining #define will automaticaly be the only one
  - Ultimately this results in multiple slightly different ASTs
  - Which we need to merge; e.g. if two function declarations differ only in
    calling convention then we should combine that into one with a conditional
    calling convention field (depends on the various #if conditions)
    e.g. list of calling conventions associated with their condition expressions

  a) duplicate token buffers, parse into separate different ASTs, merge ASTs
     - extract and combine the #if conditions that lead to the target #define,
       this will be the AST's condition
     - then solve out all the unreached #if/#else blocks
  b) copy tokens following multiple possible code paths into each of these code
     paths, e.g.
	#ifdef _WIN32
		#define DLL_CALLCONV __stdcall
		void DLL_CALLCONV FreeImage_DeInitialise(void);
	#else
		#define DLL_CALLCONV
		void DLL_CALLCONV FreeImage_DeInitialise(void);
	#endif
     then such #defines could be solved out trivially again. Requires a "merging
     back" algorithm of course, but that could be useful, because it would also
     handle cases such as this:
	#ifdef _WIN32
		void __stdcall f(void);
	#else
		void f(void);
	#endif

     2. Trivially expanding the #defines:
	For each #if/#else block
		for each directly contained previous #define
			expand corresponding TK_ID's in the block's trail code

     3. Merging back in the final AST:
         For each #if/#else block, from inner-most to outer-most:
             Combine similar AST nodes from #if/#else paths into one node behind
             the #endif

	this is the same algorithm needed to merge ASTs of different versions...

- macro expansion should build a list of known macros, and add more as #defines
  are found, and expand on other tokens, instead of looking for #defines and
  expanding each one separately
  because we shouldn't expand inside #define bodies, because it might accidentially
  expand a macro parameter or a TK_ID that will be merged with something.
  i.e we should expand inside bodies after expanding them, not before.
  Checking TK_ID's whether they're macro paramters or will be merged could
  work too but it's more difficult...
  Of course the question is, can we keep this scoped expansion behaviour (only
  expanding inside a certain #if/#else/#endif code path when having a list of
  known #defines...
  - store #if nesting level on each #define in that list, then it's like a
    stack: we can remove #defines when leaving the #if block.

- re-#defines are allowed in CPP as long as the macro is exactly the same, right?
  need to allow re-#defines, but oops() on invalid ones -> hAreMacrosEqual() function

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

- ## PP merging should allow TK_ID ## TK_DECNUM, currently it only allows TK_ID ## TK_ID

- escaped PP newlines (\EOL) should be solved out when parsing the macro body
