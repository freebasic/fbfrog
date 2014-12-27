typedef struct A a;

void f1(a *);
void f2(struct A *);

struct A {
};

void f3(a *);
void f4(struct A *);

/*

type A as A_

declare sub f1(byval as A ptr)
declare sub f2(byval as A ptr)

type A_
end type

declare sub f3(byval as A ptr)
declare sub f4(byval as A ptr)

*/
