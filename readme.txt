
  fbfrog -- FreeBASIC binding generator
  Copyright (C) 2011 - 2014  Daniel C. Klauer <daniel.c.klauer[at]web.de>

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


Features:

  This program reads in *.h files (C API declarations) and generates
  corresponding *.bi files (FreeBASIC API declarations). It automates most of
  the work needed to create and maintain FB bindings to C libraries, most
  importantly: converting C declarations to FB code.

  * C pre-processor (CPP)
      * tracks #defines/#undefs and does macro expansion (can be disabled for
        individual symbols)
      * preserves #define statements (can be disabled for individual symbols)
      * expands #includes (always, so it gets to see #defines/#undefs, but the
        #included code can be removed before it's given to the C parser)
      * evaluates #if blocks (always), based on an #if expression parser
      * allows for pre-#defines etc. to be specified

  * C parser
      * Recursive declaration parser that can handle multiple declarations in
        the same statement, nested declarations, including function pointers
        returning function pointers etc. Also parses some __attribute__'s, e.g.
        calling conventions.
      * Expression parser that's used to parse initializers, enum constant
        values, #define bodies. Includes parsing for type casting expressions,
        based on a heuristic (whether the identifier in parentheses looks like a
        type or is a known typedef, etc.).
      * Parses typedefs, structs, unions, enums, including nested
        structs/unions, and even struct/union/enum bodies specified directly in
        declarations of functions, parameters, variables, etc.

  * FB binding creation
      * C's built-in data types (using the sizes typically used for x86) and
        also typedefs such as size_t or int32_t are directly converted to
        corresponding FB data types. <signed|unsigned char> is translated to
        BYTE|UBYTE, while plain <char> is assumed to mean ZSTRING.
      * C expressions are converted to FB ones, as pretty as possible without
        changing result values.
      * Array parameters are turned into pointers.
      * Anonymous structs are given the name of the (first) typedef that uses
        them, and the typedef is removed.
      * Redundant typedefs (<typedef struct A A;>) are removed, because in FB
        there are no separate type/tag namespaces. <struct A> or <A> translate
        to the same thing.
      * #defines nested inside struct bodies are moved to the toplevel. Even
        though FB doesn't scope #defines inside UDTs (only inside scope blocks,
        but not namespaces), this is better, because it represents the original
        header's intentions more closely. It becomes more important when
        converting #defines to constant declarations, because FB does scope
        those inside UDTs.
      * #defines are turned into constants automatically, if the macro body is
        just a simple constant expression (can be disabled with -noconstants).
      * Conflicting identifiers (conflicts with FB keywords, or amongst the
        symbols declared in the binding, possibly due to FB's case
        insensitivity) are fixed by appending _ underscores to the less
        important symbol (e.g. renaming #defines is preferred over renaming
        procedures). For renamed variables/procedures, ALIAS "<original-name>"
        will be emitted.
      * Extern blocks are added around the binding's declarations, for the
        calling convention that the binding uses most often. Calling conventions
        and case-preserving ALIASes are only emitted on procedures if they're
        not covered by the Extern block (happens if a binding's procedures use
        multiple calling conventions).
      * Multiple parsing passes produce multiple ASTs, each representing the
        binding for a certain target system, e.g. win32 and linux, or for a
        certain version of input headers, e.g. v1.0 and v2.0. These ASTs are
        then merged into one to produce the final binding. Declarations that
        exist in one version only, or can be used on a certain target only, are
        put inside #if blocks, e.g. <#if __MYLIB_VERSION__ = 1> or
        <#ifdef __FB_WIN32__>. To select the API for a specific version, the
        binding's user can #define __MYLIB_VERSION__ as wanted.


Compiling:

  $ fbc *.bas -m fbfrog


Running the tests:

  1. $ fbc tests/run.bas
  2. $ tests/run
  3. Use Git to check the status of the tests/ directory. Any changes indicate
     test failures.


Usage:

  Pass *.h files (C API declarations) to fbfrog:
    $ ./fbfrog foo.h
  and fbfrog generates a corresponding *.bi file (FB API declarations).

  Creating a binding for multiple versions of a library:
    $ ./fbfrog -version 1.0 foo1.0.h -version 2.0 foo2.0.h

  Creating a binding for GCC 4 Win32/Linux versions of a header:
    options.txt:
        -define __GNUC__ 4
        -target win32
            -define _WIN32
        -target linux
            -define __linux__
    $ ./fbfrog foo.h @options.txt

  -version can be used to tell fbfrog about multiple versions of a binding.
  -target is similar and causes fbfrog to parse multiple versions of the
  binding as they would be used when compiling for the respective systems.
  -targets can be nested in -versions. fbfrog will parse input files registered
  for each version/target, do preprocessing with the symbols registered for each
  version/target, and combine the resulting declarations into a single binding.


To do:

- support defined() in #define bodies (C expression parser)
- support macro expansion for C/CPP keywords, not just TK_ID

- Turn -nomerge into -insert <absolute-file-name-pattern>
  By default: Don't insert #includes
  Can use -insert '*' to insert all #includes that can be found,
  or -insert '*/gtk/*' to insert only certain ones, etc.
    - Seems nicest to match against the absolute path but with common prefix
      removed already. #include messages should use the stripped filename, and
      show whether the #include will be inserted or not - that will help the
      user to tell what the pattern being matched against, and whether it
      matched or not.
- If an #include was found but not inserted, then the #include statement should
  be preserved (but renamed .h -> .bi)
- #includes that can't be found should just be removed, e.g. system headers
- fbfrog should automatically add the proper crt/* #include if needed

- consider splitting statement vs. expression AST into 2 sorts of nodes
  statements = array of statements
  expression = binary tree

- Performance issues:
  - speed: AST walking, especially astReplaceSubtypes()
      - Could be fixed by using a type map - then astReplaceSubtypes() would only
        need to update an entry in the type map instead of walking the whole AST
  - speed: String comparisons (also as part of AST walking)
  - memory usage: hAstLCS() matrix; also string/AST node/token buffer allocations

- 64bit support:
  * Add arch-specific pre-#defines too
  * Enums must be emitted as Long for 64bit compat:
    a) On all enum bodies, do "enum Foo as long : ... : end enum" (must be added to FB first)
    b) Do "type Foo as long" and make enum body anonymous (enums consts aren't type checked anyways)
    c) Do "const EnumConst1 as long" for every enum const; don't emit the enum type at all,
       do "long" in place of every "enum Foo" type. (would have to calculate enumconst value
       if no initializer given)

- #if expression evaluation:
  - Support unsigned values
  - Don't crash on (INT_MIN / -1) or (INT_MIN % -1)

- Should astOpsC2FB() insert clng() at various places to ensure that C int
  expressions produce 32bit values even in 64bit FB?

- char array should be translated to zstring * N:
    static char s[10] = "hello";
    dim shared s as zstring * 10 => "hello"
- macro params named after keywords?
- parentheses around macro params should be preserved (can use a flag on the AST node)
- Emit list of renamed symbols at top of header
- Add -booldefine to mark a macro as "returns a bool", so the C #define parser
  can set is_bool_context=TRUE when folding
- Lexer doesn't show error code context
- Better error reporting: Single error token location isn't enough - especially
  if it's EOF or part of the next construct, while the error is about the
  previous one. Need to report entire constructs (token range), or for
  expressions visualize operand(s) & operator, etc.
    - Let caller determine construct boundaries? They're different for
      cmdline/cpp/c anyways. -> pass in construct token range
    - E.g. C usually wants to complain about construct, but CPP usually about
      macro body or directive etc.
    - pass in additional token ranges: unexpected token + what was expected,
      operator, operands
    - Show suggestions how to fix errors, e.g. if #define body couldn't be parsed,
      suggest using -removedefine to exclude the #define from the binding...
- Comments given to a TK_ID that is a macro call and will be expanded should
  be given to first non-whitespace token from the expansion, for example:
        // foo
        CALLCONV void f(void);
- comments behind #define bodies should go to the #define not the body tokens
- Add tk array implementation, and compare performance, on headers with lots
  of macro expansion.

- Write fbc wrapper that would allow using fbfrog as fbc pre-processor:
  Search for
    #fbfrog <fbfrog command line>
  and replace it with the generated binding code (in a temp copy of the input
  .bas file). Of course it will only recognize stuff from the fbfrog command
  line, not any FB #defines appearing above the #fbfrog statement...
