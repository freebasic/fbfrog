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
  #includes for `crt/long[double].bi` or `crt/wchar.bi` are automatically added if needed.
* Named enum => `type enumname as long` + anonymous enum, because C enums/ints stay 32bit on 64bit,
  so in FB we have to use the always-32bit LONG type instead of the default ENUM/INTEGER type.
* Function/array typedefs (not supported in FB) => solved out
* struct/union/enum tag names are solved out in favour of typedefs, if any.
  Exact-alias typedefs (`typedef struct A A`) or case-alias typedefs (`typedef struct a A`) are solved out, since FB doesn't have the separate tag namespace and is case-insensitive anyways.
* Anonymous structs (not supported in FB) => named after first typedef that uses them, or auto-generated name
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
