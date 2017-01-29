## fbfrog Overview

* fbfrog acts a lot like a C compiler, and by default it pretends to be gcc
  compiling for the various FB targets (win32, win64, linux-x86, linux-x86_64, etc.).
  fbfrog uses mostly the same pre-#defines and data types as gcc. Much of these
  can be found in `include/fbfrog/default.h`, the rest is hard-coded in the C parser.
* fbfrog parses the headers top-down like a C compiler, so it's only necessary
  to pass the "entry points", i.e. the header(s) that would be #included in a C program.
  fbfrog expands all #includes it can find.
  The generated binding will cover the API that would become available by
  #including those headers in the given order. Separate headers that aren't
  intended to be #included together shouldn't be passed to fbfrog together, but
  in separate invocations.
* If fbfrog can't find #included header files, you can use the `-incdir <path>`
  option to help it. Sometimes this is needed to allow the main header to be parsed successfully.
  (For example, a macro can only be expanded if the #define statement was seen.)
* Standard system headers (e.g. from C runtime or POSIX) will often be reported
  as "not found", which is ok. Usually they are not needed when generating a
  binding for some library. Standard header search directories like `/usr/include`
  should not be used for binding generation, because they are system-specific,
  while FB bindings usually are supposed to be portable.
* fbfrog preprocesses and parses the input headers multiple times: once for each
  supported target (DOS/Linux/Windows/etc, x86/x86_64/arm/aarch64) and merges
  all these APIs together into the final binding. If you need to override this
  (for example if your .h files don't support DOS and have an #error statement
  for this case), then use -target and specify the needed targets manually.
* fbfrog has a custom C preprocessor and parser with some unusual features:
    * it preserves #defines and parses their bodies
    * it can expand macros and #includes selectively/optionally
    * pre-#defines are fully configurable, no hard-coded target/compiler-specifics
    * modifyable AST to make it FB-friendly and insert FB-specific constructs
* fbfrog needs a C preprocessor, because most C library headers make extensive
  use of macros, usually for specifying the calling convention or other attributes
  on function declarations, but in some cases the whole function declaration is
  hidden behind multiple layers of macros, such that it would be nearly impossible
  to determine the API without doing macro expansion. With the need for macro expansion
  comes the need for parsing #defines and evaluating #if blocks. This leads to the multiple
  target-specific interpretations of a C header, which is exactly how it would be seen by a
  C compiler when compiling for different targets.
* In practice, many C libraries have target-specific headers that are generated
  during the compilation of the library. This makes it hard to produce an FB binding
  that covers all targets: In general it's necessary to get the proper C headers for each target,
  and feed them into fbfrog such that it can combine them into one. Sometimes it's possible
  to work-around the problem by using preprocessor tricks or some hand-written fake .h files.
