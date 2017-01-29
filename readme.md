## fbfrog: A binding generator for FreeBASIC

fbfrog is a command line tool which reads .h files (C API declarations) and generates corresponding .bi file(s) (FreeBASIC API declarations).
This tool was needed because the FreeBASIC Compiler is lacking the ability to #include C headers directly,
and instead requires all data type and function declarations to be translated to the FB syntax. At least fbc is ABI-compatible to gcc,
so only the translated headers are needed, no binary wrappers (for C++ bindings it's a different story though). fbfrog doesn't support C++.

1. Compile fbfrog via `make` or `fbc *.bas -m fbfrog`. The test suite can be run via `make tests` and then `git diff`.
2. Run fbfrog and pass the *.h file(s) that you want to #include on the command line:

    ```
    ./fbfrog foo.h
    ```

3. Check the produced .bi file. It needs to be reviewed and tested. TODOs may have
   to be fixed up manually, or require special options like `-incdir` (to find missing #includes)
   or `-define WINAPI __stdcall` (to expand missing preprocessor macros), and re-run.
   Often TODOs are caused by untranslatable #defines. If not needed, they can be ignored
   or removed with `-removedefine`.


## Examples


#### zlib

```
wget http://zlib.net/zlib-1.2.11.tar.xz
tar xf zlib-1.2.11.tar.xz

./fbfrog zlib-1.2.11/zlib.h \
  -renametypedef Byte Byte_ \
  -renametypedef uLong uLong_ \
  -renamedefine zlib_version zlib_version_
```

#### bzip2 library

```
wget http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz
tar xf bzip2-1.0.6.tar.gz

./fbfrog bzip2-1.0.6/bzlib.h \
  -define WINAPI __stdcall \
  -removeinclude windows.h \
  -renameproc BZ2_bzread BZ2_bzread_ \
  -renameproc BZ2_bzwrite BZ2_bzwrite_
```

#### freeglut library

```
wget http://sourceforge.net/projects/freeglut/files/freeglut/3.0.0/freeglut-3.0.0.tar.gz/download -O freeglut-3.0.0.tar.gz
tar xf freeglut-3.0.0.tar.gz

./fbfrog \
  -incdir freeglut-3.0.0/include \
  -include GL/freeglut.h \
  \
  -emit '*/GL/freeglut.h'     freeglut.bi \
  -emit '*/GL/freeglut_ext.h' freeglut_ext.bi \
  -emit '*/GL/freeglut_std.h' freeglut_std.bi \
  \
  -define FREEGLUT_LIB_PRAGMAS 1     \
  -define GLUT_DISABLE_ATEXIT_HACK 1 \
  -define NDEBUG 1                   \
  -iftarget windows                  \
    -declarebool FREEGLUT_STATIC     \
    -ifdef FREEGLUT_STATIC           \
      -define FREEGLUT_STATIC 1      \
    -endif                           \
  -endif
```


## More details

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


## Automatic Translation Features

#### Syntax

* C and FB are similar enough to allow most declarations to be converted 1:1 by
doing a pure syntax conversion, for example:

    ```
    struct UDT {              =>    type UDT
        float f;              =>        f as single
    };                        =>    end type
    void f(struct UDT *p);    =>    declare sub f(byval p as UDT ptr)
    ```

* More complex C syntax cases:

    ```
    Multiple declarations in single statement (FB is less flexible than C here):
      extern int a, b, *c, d(void);
    =>
      extern as long a, b
      extern as long ptr c
      declare function d() as long

    struct/union/enum bodies declared as part of other declarations:
      typedef struct { ... } A, *PA;
    =>
      struct temp1 { ... };
      typedef struct temp1 A;
      typedef struct temp1 *PA;
    =>
      type temp1 : ... : end type
      type A as temp1
      type PA as temp1 ptr
    =>
      type A : ... : end type
      type PA as A ptr

    Nested named UDT bodies are moved outside of the parent UDT (not supported in FB).
        struct A {
            struct B { }; /* gcc warning: declaration does not declare anything */
        };
    =>
        struct B { };
        struct A { };

    Nested function pointers:
      extern int (*(*f)(int (*a)(void)))(int b);
    =>
      extern f as function(byval a as function() as long) as function(byval b as long) as long
    ```

* Toplevel C assignment expressions are turned into FB assignment statements,
  even wrapped in scope block if it's inside a macro body, to enforce its use as
  statement, not expression. Otherwise, assignments could be mis-used as comparisons.
* Toplevel comma operators are translated to a statement sequence:

    ```
    Example of statement or macro body with comma operators:
      (a, b, c)
    =>
      scope
          a
          b
          [return?] c
      end scope
    ```

* Unnecessary scope blocks (e.g. nested inside loop blocks) are solved out.
  C if blocks nested in else blocks are converted to FB elseif blocks.
  `while (0) ...` or `do ... while (0)` blocks are turned into FB scope blocks.
  Sometimes this can clean up macros or inline functions; rarely useful though.

#### Data types and structures

* The normal C data types and some common typedefs such as `size_t`, `int32_t` or `intptr_t` are translated to normal FB data types.
  There is special support for translating `char => byte`, `char* => zstring ptr` and `char[N] => zstring * N` (same for `wchar_t => wstring`).
  char/wchar_t typedefs are expanded in case they are used as string in some cases and byte in others.
  The `-string`/`-nostring` options can be used to override the automatic conversion.
  C's `long` and `long double` types are translated to `clong` and `clongdouble`.
  #include for `crt/long[double].bi` or `crt/wchar.bi` are automatically added if needed.
* Named enum => type enumname as long + anonymous enum, because C enums/ints stay 32bit on 64bit,
  so in FB we have to use the always-32bit LONG type instead of the default ENUM/INTEGER type.
* struct/union/enum tag names are solved out in favour of typedefs, if any.
  Case-alias typedefs are solved out since FB is case-insensitive anyways.
  FB doesn't have the separate tag namespace.
* Function/array typedefs (not supported in FB) => solved out
* Anonymous structs (not supported in FB) => named after first typedef that uses them, or auto-generated name
* typedef struct FOO FOO; => solved out (FB doesn't have separate type/tag namespaces)
* Forward-references to tags/types are handled by auto-adding forward declarations,
  but only if the referenced tag is actually declared in the API. This way,
  tags/types from other headers like `FILE`/`jmp_buf`/`time_t` or `struct tm` won't be
  forward-declared.

#### Macros and preprocessor directives

* Exact-alias-#defines (`#define A A`) are removed (neither needed nor possible in FB).
  Case-aliases (`#define a A`) are solved out since FB is case-insensitive anyways.
* #defines with simple constant expression in their bodies => FB constants
* Alias-#defines for constants/types/functions/variables are converted to declarations,
  using the ALIAS keyword where needed.

    ```
    const A = ...
    #define B A    =>  const B = A

    type A as ...
    #define B A    =>  type B as A

    declare sub/function A
    declare sub/function C alias "X"
    #define B A    =>    declare sub/function B alias "A"
    #define D C    =>    declare sub/function D alias "X"

    extern A as ...
    extern C alias "X" as ...
    #define B A    =>    extern B alias "A"
    #define D C    =>    extern D alias "X"
    ```

* Macro parameters which conflict with FB keywords or other identifiers (due to
  FB's case-insensitivity) in the macro body are automatically renamed (because fbc can't catch this as
  "duplicate definition" like name conflicts between symbol declarations).
* #defines nested inside struct bodies => moved to toplevel (helps when converting #defines to constants, because FB scopes those inside UDTs)
* `#define m(a, ...) __VA_ARGS__` => `#define m(a, __VA_ARGS__...) __VA_ARGS__`
* `#pragma comment(lib, "foo.lib"|"libfoo.a")` => `#inclib "foo"`
* `#include` statements are generally preserved if not expanded; .h is replaced by .bi.

#### Variables, Functions, Parameters

* Most used calling convention => Extern block. Other calling conventions (if header uses multiple ones) are emitted on the individual procedures.
  Extern blocks are also used to avoid the need for explicit case-preserving ALIAS'es on any extern declarations.
* Array parameters (not supported in FB) => pointers (what they become in C behind the scenes anyways)
* Special case for the occasionally used `jmp_buf` parameters: They're explicitly converted to pointers,
* Arrays/strings declared with unknown size => "..." ellipsis
  because fbfrog usually doesn't see the CRT headers. `jmp_buf` is an array type in C, i.e. passed as pointer. `jmp_buf` is a UDT in FB's CRT binding.
* Unsized extern array variables aren't allowed in FB, and require some tricks to be translated.
  However, if the array size is known, it's better to use `-setarraysize` to specify the exact array size, and get a cleaner translation.

    ```
       extern dtype array[];
       extern char s[];
    =>
       extern array(0 to ...) as dtype
       extern s as zstring * ...
    =>
       #define array(i) ((@__array)[i])
       extern __array alias "array" as dtype
       extern __s alias "s" as ubyte;
       #define s (*cptr(zstring ptr, @__s))
    ```

* Simple (inline) functions are converted to macros, because FB doesn't have "proper" inline functions.

#### Expressions

* Boolean operations result in `1|0` in C. This is converted to FB's `-1|0` by inserting a negation if used in math context. C's logical NOT `!x` becomes FB's `x = 0`.
* All 32bit unsigned int IIFs/BOPs/UOPs are wrapped in culng()/clng() casts,
  in order to make sure the result is truncated to 32bit properly in FB even on 64bit,
  where FB will do 64bit arithmetic.

    ```
    For example, in C:
       (0u - 100u)   =>    0xFFFFFF9Cu
    but in FB:
       (0ul - 100ul) =>            &hFFFFFF9Cu (32bit)
       (0ul - 100ul) =>    &hFFFFFFFFFFFFFF9Cu (64bit)
    and &hFFFFFFFFFFFFFF9Cu <> &hFFFFFF9Cu (no sign extension due to unsigned).
    In order to make sure we always get &hFFFFFF9Cu in FB, we have to truncate
    the operation's result to 32bit explicitly.
    ```

* When type-casting string literals, an @ (address-of) operator is inserted, because string literals
  automatically become pointers in C, but not in FB.
* `(void)` casts, which are common to ensure a function call or other expression can only be used as a statement,
  are automatically removed, because it's probably not worth it to translate to FB,
  even though it could be done by wrapping the statement in a scope block.

#### Error handling

Declarations which cannot be processed automatically (yet) will be embedded into
the *.bi file in form of a "TODO" comment, for example: `'' TODO: #define FOO ...`.
This affects complicated #defines ("arbitrary" token sequences),
function bodies (inline functions), plus some others such as language features
or compiler extensions not supported by fbfrog's parser (yet).


## Extra features

fbfrog has various options for manual improvements, for example for...
* Renaming symbols, which is useful to resolve name conflicts.
  This is a common problem due to FB's case-insensitivity and different namespacing (e.g. #defines collide with functions).
  When using the renaming options, auto-generated list of renamed symbols are added to the top of affected header files.
* Translating #define bodies as token sequences, instead of trying to parse as expression (which is useful in specific cases).
* Removing declarations by name or type.
* Expansion of any typedef by name
* Specifying the size of unsized arrays
* Specifying hints about which identifiers are typedefs, which helps the C parser's
  type cast expression parsing when not all typedefs were declared yet. Sometimes C headers
  use typedefs from other headers, so fbfrog may not get to see the typedef declarations.
  Or a typedef may be used in a #define body before being declared.
* etc., run `./fbfrog` to see list of options

In general, this covers things that can't be decided automatically, but require human intervention.


## About the -declare*/-select/-ifdef options

fbfrog is able to read multiple headers or multiple versions of the same
header (preprocessed differently) and merge them into a single binding.
1. This is used to support multiple targets (DOS/Linux/Win32, x86/x86_64): Instead of looking for #ifs in the input headers and possibly trying to preserve those, fbfrog preprocesses and parses the input header files multiple times (using different predefines each time), and then merges the resulting target-specific APIs into one final binding, by (re-)inserting #ifs (such as `#ifdef __FB_WIN32__`) where needed.
2. By using the `-declare*` command line options you can combine pretty much any APIs, for example version 1.0 and 2.0 of a library, or the ANSI and UNICODE versions of a Win32-specific header. Of course it only makes sense if the APIs belong together. Sometimes the merging algorithm produces a rather ugly result though, especially if the differences between the APIs are too big, so it's not always useful.

Assuming we have the header files foo1.h and foo2.h, let's use the following
fbfrog options:

```
-declareversions __LIBFOO_VERSION 1 2
-selectversion
-case 1
    foo1.h
-case 2
    foo2.h
-endselect
```

Save those options into a foo.fbfrog helper file (because it's too much to
type at the command line), and pass it to fbfrog:

```
./fbfrog foo.fbfrog
```

The created binding will allow the user to #define __LIBFOO_VERSION to 1 or
2 in order to select that specific API version:

```
[...declarations that existed in both foo1.h and foo2.h...]
#if __LIBFOO_VERSION = 1
    [...declarations that existed only in foo1.h...]
#else
    [...declarations that existed only in foo2.h...]
#endif
[...etc...]
```

You can use -declare* options as wanted to support multiple APIs in 1 binding:

```
-declareversions <symbol> <numbers...>
    Useful to allow selecting an API by version. This will produce #if
    blocks such as #if <symbol> = <number>.

-declarebool <symbol>
    Useful to allow API selection based on whether a certain symbol is 
    defined or not. For example, this could be used to support 
    distinguishing between UNICODE and ANSI versions of a binding 
    (-declarebool UNICODE -> #ifdef UNICODE) or the shared library/DLL 
    version and the static library version, etc.
```

If multiple -declare* options are given, they multiply. For example, `-declarebool A -declarebool B` produces these APIs:

```
     defined(A)  and      defined(B)
     defined(A)  and (not defined(B))
(not defined(A)) and      defined(B)
(not defined(A)) and (not defined(B))
```

You can use the -select/-ifdef logic options to create different "code paths"
where some options will only be used for some APIs (instead of applying to
all APIs). This also works with -declare* options, allowing you to build
even complex API condition trees.


## Bugs
* In FB, anon UDTs inherit their parent's FIELD alignment, that's not gcc-compatible.
  fbfrog needs to generate FIELD=8 on anon UDTs if the parent has a FIELD but the anon doesn't.
  http://www.freebasic.net/forum/viewtopic.php?f=3&t=19514
* C parser needs to verify #directives, since they can be inserted by "to c" -replacements,
  which aren't verified by the CPP.
* In winapi, there is a case where an auto-generated tagid conflicts
  with a real typedef, which is errornously renamed. Luckily fbc detects this
  problem easily (recursive UDT).

    ```
    struct Foo {
      struct {
        HWND hwnd;
      } HWND;
    };
    ```

* fbfrog produces `wstr("a") wstr("b")` which isn't allowed in FB; fbfrog needs to insert `+` string concat operators.


## Old to-do list
* -1to1 option which automatically adds -emit options for each input .h such
  that each .h is emitted into its own .bi, in the directory given with -o. Strip only the common prefix, preserve remaining directory structure (if any).
* Define2Decl shouldn't move all alias defines - it's typically unnecessary for procs/vars/typedefs at least.
* Define2Decl shouldn't count #undefs as declarations (preventing affected symbols from being handled by the pass)
* Define2Decl should count multiple, equal declarations as one declaration
* Only add things to renamelist if they have a RENAMED flag (not everything with an alias was renamed)
* Add -printcconstruct <pattern> option for dumping C constructs as seen by fbfrog
  to make writing replacements easier. (TODOs aren't enough, because sometimes we
  want to do a replacement even though it's not a TODO)
* don't build VERAND conditions at frogEvaluateScript() time, but rather do it
  later when generating the #if conditions. -declareversions/-declarebool should store version number/flags in ApiInfo,
  and frogEvaluateScript() should build ApiInfo objects directly, then copy them for recursive invocations, no more separate loadOptions().
* LCS algorithm is main performance bottle-neck (especially for Windows API binding), can it be optimized?
* Turn more inline functions into macros: also void functions whose body can
  just be used as macro'd scope block, and doesn't contain any RETURNs.
* It would be nice if fbfrog could preserve comments for documentation purposes.
* Flatten AST data structure such that statements nested inside struct/proc bodies
  can be merged separately from the compound block (i.e. use TYPEBEGIN/TYPEEND/PROCBEGIN/PROCEND nodes).
  Interesting for merging the fields if UDT's FIELD=N value changes between targets.
* Don't expand macro constants outside CPP expressions, to keep them as array size etc.
* Solve out tag ids if there is an alias typedef, unless the tag id is used elsewhere
* Add pattern-based renames, e.g. `-renamedefine '%' 'FOO_%'`,
  or at least `--rename-define-add-prefix '*' FOO_` (add prefix FOO_ to matching defines).
* Auto-convert C's [] array indexing into FB's (): track which vars/fields are
  arrays (or pointers) and then compare indexing BOPs against that.
* Add support for `#pragma pack` with named stack entries (`#pragma {pack|pop}(push, <identifier> [, N])`)
  Popping by name means popping everything until that node is popped. If not found, nothing is popped.
  (MinGW-w64 CRT headers use this)
* Continue support for parsing function bodies: `++` and `--` operators, for loops, continue/break, goto/labels/switch/case.
* Add some C++ support
* ...

## Future improvements
Nowadays, I think it would be best to work on adding a C parser to fbc (i.e. the ability
to #include .h files into FB programs), making fbfrog unnecessary.
* Advantage for users: no more outdated/incompatible/missing bindings, no more binding maintenance.
* Advantage with regards to binding generation: fbc only has to deal with one target system or library version at a time, no more parsing 20 times and slow merging.
* fbc could allow specifying translation hints to handle TODOs if needed. It could come with a set of hints for common libraries. This is the same idea as with fbfrog options.
* Only C-to-FB, no FB-to-C interaction, except maybe trivial #defines to give access to user-configurable parts of the C headers.
