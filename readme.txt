FB frog - the wacky h2bi type of translator

We should:
> Read in all .h's from the command line and spit out corresponding .bi's
> Not have configuration options (always do the right thing automatically),
  instead the tool itself may be modified, it's small enough for that,
  and this isn't even more work than figuring out some sort of scripting
  language or rules specifications or something.
> Preserve preprocessor directives/unexpanded macros (can't use a real C parser)

> --combine: lex_insert_file() the main file, scan for #includes and insert
  them via lex_insert_file(), even nested #includes, but drop recursive
  #includes. Then go to normal (single file) translation & emitting.
  Input files that aren't #included are just appended to the main file,
  #includes that aren't listed among input files are ignored.

> Show message after emitting:
  foo.bi: 14 TODOs, 512 lines (60 KiB) from 332 lines (53 KiB)

> #define tracing
  For example, many headers use defines like <WINAPI> (defined to <__stdcall>)
  in function declarations. We should backtrack what those expand to, so we can
  tell whether it's a calling convention (these are most important),
  or something else, only to know where to place it in the FB declaration.
  If the #define isn't found in the current file, we should scan the other files
  and restart the translation process...
  And/or we could use multiple token buffers.

> If something really cannot possibly be automatically determined, not even
  with multiple parsing runs, we could let the user add "rules" or "hints"
  into the C headers, as comments, so they can improve the translation and/or
  reduce number of TODOs, etc.
  For example:
	DROP := __attribute__((noreturn))
	__attribute__((noreturn)) int bam();

> Hash the token text (good especially for e.g. TK_SPACE)

> #if/#include token branches (C constructs split up across #if/#else branches)
  There are many possible quirks. C constructs can be split up across one or
  more #if/#else branches, even with nested #ifs. This could only really be
  handled by parsing each PP code path into C constructs and then finding out
  whether it's ok to leave it as is, or whether tokens from the outside might
  need to be moved & duplicated to complete the constructs inside each branch,
  afterall FB doesn't allow PP in the middle of a function declaration etc.

> Check for nested /' or '/ in C comments, those need to be replaced
  since they have meaning in FB...

