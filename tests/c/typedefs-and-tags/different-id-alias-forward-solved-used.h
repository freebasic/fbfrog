typedef struct B A;

struct B {
	A* a;
};

void f(A *);

/*

type A as B

type B
	a as A ptr
end type

declare sub f(byval as A ptr)

*/
