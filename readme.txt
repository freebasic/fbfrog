FB frog - the wacky h2bi type of translator

We should:
> Read in all .h's from the command line and spit out corresponding .bi's
> Not have configuration options (always do the right thing automatically),
  instead the tool itself may be modified, it's small enough for that,
  and this isn't even more work than figuring out some sort of scripting
  language or rules specifications or something.
> Preserve preprocessor directives/unexpanded macros (can't use a real C parser)

> Only one token buffer at a time
  - If we want to gather certain information like typedef vs struct usage,
    then we can read in, tokenize and analyze the input files multiple times,
    before finally reading it in for the last time and actually translating it.
  - This is ok for combining multiple files

> --combine: lex_insert_file() the main file, scan for #includes and insert
  them via lex_insert_file(), even nested #includes, but drop recursive
  #includes. Then go to normal (single file) translation & emitting.
  Input files that aren't #included are just appended to the main file,
  #includes that aren't listed among input files are ignored.

> Show message after emitting:
  foo.bi: 14 TODOs, 512 lines (60 KiB) from 332 lines (53 KiB)

