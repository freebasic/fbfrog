typedef struct A a;

void f1(a *);

struct A {
};

void f2(a *);

/*

type a as a_

declare sub f1(byval as a ptr)

type a_
end type

declare sub f2(byval as a ptr)

*/
