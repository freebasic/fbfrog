fbfrog -- a h2bi translator

The idea is to base the translation process not on parsing C into a tree,
but rather on rearranging/translating tokens and the constructs they form.
It can easily translate "int a;" to "as integer a", but it cannot that easily
rearrange whole expressions if FB operators work differently than C ones.
It can preserve whitespace, commentary and preprocessor directives, but it
cannot translate constructs obscured by macros.

The results look very much like the original. Anything it couldn't handle
is marked with a TODO comment.


Usage:

First, compile it using fbc, for example:
	fbc -m fbfrog *.bas

Translating headers:
	./fbfrog foo.h

Automatically look for #includes in headers and recursively translate all
f them that can be found:
	./fbfrog --follow start.h

Combine everything into a single main header if possible:
(Merge in #included headers if not used anywhere else,
concatenate toplevel headers that don't #include each other)
	./fbfrog --follow --merge --concat *.h

For options, see also:
	./fbfrog --help


Here's how it works:

The core is a huge dynamic array of tokens. Each token holds this information:
  - token id (keyword? identifier? comma? eol? comment? space? etc.)
  - associated text (for identifiers, strings, numbers)
  - construct id (mark; tells what language construct this token belongs to)

The token buffer is filled by a C lexer, which reads in the input .h files
and inserts the corresponding tokens into the token buffer.

At any time, the token buffer can be emitted into a file. That's as simple as
going through all tokens and just writing them out, no matter what they are.

The token buffer is accessed through functions that take an index (often
called x) and return information on the token at that position. It works just
like an array, so there is no "current position" or anything. All tokens can
be accessed at any time, which allows for easy look-ahead/back-tracking.

Parsing is the pass that goes through the token buffer to identify constructs,
setting the token marks accordingly. Unknown constructs are marked as unknown.
Since there are whitespace tokens present, the parser uses helper functions to
skip over such noise via x = skip(x), instead of just doing x += 1.

Translation is the pass that goes through all tokens, looks at their construct
marks and translates/rearranges constructs based on that.

For example, to translate a field declaration such as "int a;":
  - The fielddecl translator would remove the "int" token,
  - and insert "as integer" instead.
  - Then it would skip over the identifier "a", since there is no change
    required to translate it,
  - and finally it would remove the ";",
  - resulting in "as integer a"

Parsing and translating in two separate steps has the advantages of:
  - being able to do trial-and-error parsing: Check whether something
    is a function declaration, if it's not -- give up and try something
    else, no harm done.
  - not having to store AST-like information as the result of parsing;
    when needed, the translator can just do some parsing itself.
  - being easily able to identify and handle EOLs inside constructs, allowing
    them to be be made "FB-compatible" by inserting the "_" line continuation
    character (or just deleting them).
  - being able to do preparses to collect information on #includes and #defines.


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
  - C++ methods currently trigger a bug warning, probably because the multdecl
    splitup only sets MARK_PROCDECL for MARK_TOPDECL, not MARK_FIELDDECL,
    and the fielddecl translator comes across unexpected ()'s...

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

> Add --input-dir <path> and scan it for *.h files

> Pretty print .bi input
  - Add FB mode to the lexer, differences to C aren't that big
  - Keyword casing
  - Function declaration wrapping?
  - Fixup indendation and overhead space?

> Make a simple GUI -- someone is going to ask for it anyways
