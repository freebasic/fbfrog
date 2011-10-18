
    fbfrog -- a h2bi translator

    This program is an experiment in C-to-FB header translation.
    The idea is to base the translation process not on parsing C into a tree,
    but rather on rearranging/translating tokens and the constructs they form.

    The simple find & replace type of things can already be a great help, they
    cover number/string literals in #defines or operators in expressions for
    example, and together with the translation of enums, structs and procedure
    declarations you have the most common header translation problems covered.

    If the translator encounters a construct that it can't translate, it will
    leave it untouched, add a TODO comment so you'll find it, and continue
    behind it. However, this not a C compiler, so the input is not validated.
    All headers to translate are usually error-free anyways.

    The coolest part though is that preprocessor directives, unexpanded macros,
    documentational commentary and even indendation whitespace can be preserved.


  o Usage

    First, compile it using fbc, for example:
        $ fbc -m fbfrog *.bas

    Translating headers:
        $ ./fbfrog foo.h

    Automatically look for #includes in headers and recursively translate all
    of them that can be found:
        $ ./fbfrog --follow start.h

    Combine everything into a single main header if possible:
    (Merge in #included headers if not used anywhere else,
    concatenate toplevel headers that don't #include each other)
        $ ./fbfrog --follow --merge --concat *.h

    For options, see also:
        $ ./fbfrog --help


  o Here's how it works:

    The core is a giant array of tokens. A token has an id to tell what
    operator or keyword etc. it represents, some associated text, which is
    used by identifiers for example, and a construct/statement id that can
    be used to mark tokens as "belongs to a PP directive" or "belongs to a
    function declaration". There even are space and comment tokens, which
    is quite unusual, but the plan is to preserve them as well as possible,
    so it has to be done.

    There are several functions to retrieve the data of the token at a
    specific position, and to insert/remove a token at a specific position.
    All access is done via an index, as if it was an array, so there's no
    "current position" or anything, which allows for some nice tricks (like
    very easy look-ahead or look-back) in the parsing code.

    There's a C lexer that reads input .h files and adds the corresponding
    tokens to the core token buffer.

    There's an emitter that writes out the current tokens in the buffer
    into a file. This is used to write out the translation result into a
    .bi file, but it's also useful to dump the state half-way into the
    translation for debugging purposes.

    The main part is parsing and translating. The input file is read in using
    the C lexer. Then a parsing run goes through the whole token array and
    identifies constructs like preprocessor directives, struct/enum blocks,
    or function declarations. Tokens are marked as part of the identified
    construct, or as nothing if the parser couldn't tell what it is. After
    this, the translation process goes through the whole thing again, and
    rearranges/inserts/deletes tokens to translate the identified constructs.

    It's worth noting that the parser ignores comments/whitespace/EOLs when
    parsing the C constructs, but these tokens are still there. The translator
    cares about space a bit more; for example it ensures there is space
    between newly inserted keywords, or removes surrounding space when
    removing tokens.

    Marking the tokens and parsing/translating in separate steps has the
    advantages of:
      a) Being able to do trial-and-error parsing: Check whether something
         is a function declaration, if it's not -- give up and try something
         else, no harm done.
      b) Not having to store AST-like information as the result of parsing;
         when needed, the translator can just do some parsing itself.
      c) EOLs inside constructs like function declarations are marked as part
         of the construct, thus they can easily be identified and deleted or
         replaced by EOL + line continuation, to make them "FB-compatible".


  o To do

> More constructs:
  - Expression parser & translator that allows for better/less ambigious
    operator translation, and can handle ?: -> iif().
  - #elif -> #elseif
  - vardecl/param initializers
  - Constness CONST
  - Forward declarations
	// type T as T_
	// (this way we only need to translate the <struct T ...> body as
	// <type T_ ...>, that's easier than inserting T_ fwdref in place of T
	// everywhere where it's used before the T body)
	struct T;
  - C++ methods currently trigger a bug, probably because the multdecl
    splitup only sets MARK_PROCDECL for MARK_TOPDECL, not MARK_FIELDDECL,
    and the fielddecl translator of course won't expect the ()'s...
  - #include <foo>
  - EXTERN/callconv #defines in front of procdecls
    For example, many headers use defines like <WINAPI> defined to <__stdcall>
    in function declarations. Those should be backtracked to see what they expand
    to, so we can tell whether it's a calling convention (these are most
    important) and then put it into the right position in function declarations.
    options:
      a) Support multiple token buffers via tk_switch(buffer-index) and scan
         the files for #defines without touching the translation-in-progress
      b) Launch a second process to filter the files for those #defines, then
         parse its output.
      c) In the current file only, parse backwards to find those #defines,
         or collect a list of such #defines
      d) Collect information on such #defines during a preparse

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
  - extern "C"/cdecl alias fixups
  - Realign commentary

> Use a onewaystorage for token text. Then we don't need to worry about
  deallocating(), and tk_end() can easily clean it up too...
  we don't delete that many text tokens anyways (we /preserve/ most identifiers
  etc).
  Now that token text is never deleted we can also use a hash to hash token
  text and save some memory.
> Make each TK_SPACE correspond to a single space -- that's just easier for
  everything, including overhead space removal
> Change insert_spaced_token() to be smarter and only insert space to separate
  keywords from each other, but not from ')' etc.
  This would be easier if all keywords were KW_*, i.e. TK_NOT should be '!',
  while KW_NOT is 'NOT'
> Clean up/unify the ';' removal and ':' insertion
> Insert EOL + same indentation instead of ':' in some cases, e.g. fields or
  typede-struct-block split ups

> Add --input-dir <path> and scan it for *.h files

> Pretty print .bi input
  - Add FB mode to the lexer
  - Keyword casing
  - Function declaration wrapping?
  - Fixup indendation and overhead space?

> Make a simple GUI -- someone is going to ask for it anyways
