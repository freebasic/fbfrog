// @fbfrog -v

#define m void f(void

void ok(void);

#define A void f(
#define B() void f(
#define C(a) void f(
#define D(a, b, c) void f(

// Missing '}' inside macro body
#define M1 enum { A,
#define M2 0
