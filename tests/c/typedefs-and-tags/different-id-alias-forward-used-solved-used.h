typedef struct B A;

void f1(A *);

struct B {
	A* a;
};

void f2(A *);

/*

type A as B

declare sub f1(byval as A ptr)

type B
	a as A ptr
end type

declare sub f2(byval as A ptr)

*/
