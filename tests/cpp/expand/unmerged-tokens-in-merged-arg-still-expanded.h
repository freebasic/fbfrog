#define EXPANDME3 foo

#define EXPANDME1(x) stat##x
EXPANDME1(ic int EXPANDME3;)
static int foo;

#define EXPANDME2(x) x##123
EXPANDME2(static EXPANDME3 some_udt_variable_);
static foo some_udt_variable_123;
