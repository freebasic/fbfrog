FB frog - the wacky h2bi type of translator

We should read in all .h's from its command line and spit out corresponding .bi
files. Preprocessor directives need to be preserved as well as possible, and
we should not do macro expansion, so a real C parser can't be used.

We may consider using a tree to hold all code before emitting it, in order to
make certain decisions (e.g. how to handle structs/enums vs. typedefs, a
distinction FB doesn't have) based on whether the code uses "struct foo" or a
typedef later on. I.e. we don't need to emit as we parse.

We should have tests.

