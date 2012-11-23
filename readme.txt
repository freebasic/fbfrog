
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

  The idea is to base the translation process not on parsing C into a tree,
  but rather on rearranging/translating tokens and the constructs they form.
  It can easily translate "int a;" to "as integer a", but it cannot that easily
  rearrange whole expressions if FB operators work differently than C ones.
  It can preserve whitespace, commentary and preprocessor directives, but it
  cannot translate constructs obscured by macros.

  The results look very much like the original. Anything it couldn't handle
  is marked with a TODO comment.


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


Here's how it works:

  The core is a huge dynamic array of tokens. Each token consists of:
   - Token id (keyword? identifier? comma? eol? comment? space? etc.)
   - Associated text (for identifiers, strings, numbers)
   - Construct id (mark telling what construct this token belongs to)

  The token buffer is accessed through functions that take an index (often
  called x) and return information on the token at that position. It works
  just like an array, so there is no "current position" or anything.
  Since there are whitespace tokens present, the parser uses helper functions
  to skip over such noise via x = skip(x), instead of just doing x += 1.
  All tokens can be accessed at any time, which allows for easy look-ahead
  and back-tracking in the parsing code.

  Translating a file is done in these steps:
   1) Letting a C lexer read in the *.h file and insert the corresponding
      tokens into the token buffer.
   2) Parsing through the token buffer to identify constructs, setting the
      token marks accordingly. Unknown constructs are marked as unknown.
   3) Translating the file by going through all tokens again, and looking
      at their marks and inserting and deleting tokens as needed to convert
      the constructs to FB syntax.
   4) Emitting all tokens into a file, the output *.bi.

  Parsing and translating in two separate steps has the advantages of:
   - Being able to do trial-and-error parsing: Check whether something
     is a function declaration, if it's not -- give up and try something
     else, no harm done.
   - Not having to store AST-like information as the result of parsing;
     when needed, the translator can just do some parsing itself.
   - Being easily able to identify and handle EOLs inside constructs, allowing
     them to be be made "FB-compatible" by inserting the "_" line continuation
     character (or just deleting them).
   - Being able to do preparses (whole parsing pass, but at least without the
     translation pass) to collect information on #includes and #defines.

  For example, here is (basically) how a field declaration such as "int a;"
  would be translated:
   - The fielddecl translator would remove the "int" token,
   - and insert "as integer" instead.
   - Then it would skip over the identifier "a", since there is no change
     required to translate it,
   - and finally it would remove the ";",
   - resulting in "as integer a".

  The idea behind the --follow, --concat and --merge options is to ease the
  translation of multiple headers at once.
   - Following refers to the translation of files found referenced via
     #include directives in other files.
   - Merging is the resolving of #includes, i.e. the inclusion of the
     #included file in place of the #include directive. A file can only be
     merged in like this if it is not #included anywhere else, otherwise the
     content would be duplicated, and that is usually not nice...
   - Concatenation is supposed to be used when headers belong together and
     just should be appended into a bigger file, as a last resort, if they
     cannot be combined by merging. This is only done if the headers aren't
     #included anywhere, otherwise the files would be missing.
  All the options together can do a pretty good job at combining all headers
  into a single one, unless there are circular #include dependencies.

  To handle the #include dependencies, the fbfrog frontend keeps a list of
  (*.h) files that is filled with files from the command line and those found
  during an #include-collection preparse, with an #include reference count for
  each file.

  The preparse is not only used to collect #includes, but also to gather some
  information on #defines. Currently it just collects any #defines that contain
  something like __stdcall or __declspec(dllexport) or are empty, and the
  procedure declaration uses that information to allow and translate such
  defines in front of procedure declarations. Enabling --follow allows more
  #defines to be found, which can help. And sure enough you can just add any
  #define FOO to the top of the headers to translate, to let the preparse find
  them and help out the translation.


Source module overview:

  emit.bas           Function to write out the current token buffer into a file

  fbfrog.bas         Main module: Command line handling, header file list,
                     parsing/translation driver

  hash.bas           Generic hash table (no deletions)

  lex.bas            C lexer, exports only a function to insert a file at a
                     specific position in the token buffer

  list.bas           Generic linked list

  parser.bas         Toplevel parsing/translator functions (main loops),
                     parsing helper functions, post-translation fixup passes,
                     #define/#include parsing/merging

  parser-struct.bas  Compound block parsing (enum/struct/union blocks,
                     and the special case extern block)

  parser-decl.bas    All sorts of declaration parsing: variables, fields,
                     enum constants, procedures, parameters, typedefs,
                     everything with procedure pointers too

  pathmagic.bas      Path/file name handling functions, directory tree search

  storage.bas        Global token text buffer, allowing tokens to re-use text
                     across multiple parses, and avoiding the need to allocate()
                     and deallocate() the text on every token insert/delete.

  tk.bas             Token buffer (implemented as a gap buffer),
                     accessor functions


  gui/embed-xpms.bas    Program for regenerating xpms.bas from the *.xpm files
                        in the same directory

  gui/gtk.bas        GTK+ version of the GUI

  gui/gui.bas        GUI main module (the GUI is a separate program)
                     fbfrog launching code, some event logic

  gui/win32.bas      Win32 version of the GUI

  gui/resources.rc   Win32 resource script, compile it in to embed the icon
                     and the xp.manifest into the program

  gui/xpms.bas       Embedded .xpm icons for the GTK+ GUI


To do:

> Handle more constructs:
  - Expression parser & translator that allows for better/less ambigious
    operator translation, and can handle "?:" -> "iif()", but that's a lot
    of work
  - vardecl/param initializers
  - Forward declarations
	// type T as T_
	// (this way we only need to translate the <struct T ...> body as
	// <type T_ ...>, that's easier than inserting T_ fwdref in place of T
	// everywhere where it's used before the T body)
	struct T;

> More translations passes
  - Mark ids during translation, then check them in a global pass
  - Mark expressions during translation, for improved operator translation
    (e.g. #define bodies, or #if expressions)
  - Add a global useless-typedef-removal pass that removes typedefs that have
    the same id as a type and no pointers/procptrs on them. Add TODOs for such
    typedefs that have pointers, since those would cause dupdef errors in FB
    anyways (FB doesn't have separate struct/typedef namespaces)
	/* typedef-same-id-as-type fixups: */
	typedef struct T T;
	typedef struct T A, T;
	typedef struct T T, B;
	typedef struct T A, T, B;
  - Realign commentary

> Change insert_spaced_token() to be smarter and only insert space to separate
  keywords from each other, but not from ')' etc.
> Clean up/unify the ';' removal and ':' insertion
> Insert EOL + same indentation instead of ':' in some cases, e.g. fields or
  typede-struct-block split ups

> Pretty print .bi input
  - Add FB mode to the lexer, differences to C aren't that big
  - Keyword casing
  - Function declaration wrapping?
  - Fixup indendation and overhead space?
