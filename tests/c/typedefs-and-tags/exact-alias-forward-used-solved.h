typedef struct A A;

void f(A *);

struct A {
	A* a;
};

/*

type A as A_

declare sub f(byval as A ptr)

type A_
	a as A ptr
end type

*/
