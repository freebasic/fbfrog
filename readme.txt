FB frog - the wacky h2bi type of translator

We should read in all .h's from its command line and spit out corresponding .bi
files. Preprocessor directives need to be preserved as well as possible, and
we should not do macro expansion, so a real C parser can't be used.

We may consider using a tree to hold all code before emitting it, in order to
make certain decisions (e.g. how to handle structs/enums vs. typedefs, a
distinction FB doesn't have) based on whether the code uses "struct foo" or a
typedef later on. I.e. we don't need to emit as we parse.

Our input lexer needs to handle...
- push/pop of include files
- tokenize input for parsing
- can decipher escape sequences in string literals
- we want to use a hash table to check for problems with FB's case insensitivity
  so the lexer needs to let the parser retrieve the token text in its original
  form.
- In case of any problems we want to emit the original source code and file name
  and line number into the .bi files, so that must be available from the lexer
  too.

We should have tests.
