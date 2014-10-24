typedef struct A A;

struct A {
	A* a;
};

void f(A *);

/*

type A
	a as A ptr
end type

declare sub f(byval as A ptr)

*/
