typedef struct B A;

void f(A *);

struct B {
	A* a;
};

/*

type A as B

declare sub f(byval as A ptr)

type B
	a as A ptr
end type

*/
