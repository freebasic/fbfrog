typedef struct A a;

void f1(a *);

struct A {
};

void f2(a *);

/*

type A as A_

declare sub f1(byval as A ptr)

type A_
end type

declare sub f2(byval as A ptr)

*/
