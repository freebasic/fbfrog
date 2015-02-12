typedef struct A a;

void f1(a *);
void f2(struct A *);

struct A {
};

void f3(a *);
void f4(struct A *);

/*

type a as a_

declare sub f1(byval as a ptr)
declare sub f2(byval as a ptr)

type a_
end type

declare sub f3(byval as a ptr)
declare sub f4(byval as a ptr)

*/
