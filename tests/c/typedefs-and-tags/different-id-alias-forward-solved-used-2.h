typedef struct B A;

struct B {
	A* a;
};

void f1(A *);
void f2(struct B *);

/*

type A as B

type B
	a as A ptr
end type

declare sub f1(byval as A ptr)
declare sub f2(byval as B ptr)

*/
