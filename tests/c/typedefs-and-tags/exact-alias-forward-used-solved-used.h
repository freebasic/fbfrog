typedef struct A A;

void f1(A *);

struct A {
	A* a;
};

void f2(A *);

/*

type A as A_

declare sub f1(byval as A ptr)

type A_
	a as A ptr
end type

declare sub f2(byval as A ptr)

*/
