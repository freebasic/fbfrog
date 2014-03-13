// @fbfrog -whitespace -nonamefixup -removedefine m2

#define m3 foo

#define m1(x) stat##x
m1(ic int m3;)
static int foo;

#define m2(x) x##123
m2(static m3 some_udt_variable_);
static foo some_udt_variable_123;
