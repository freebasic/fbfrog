typedef struct B A;

void f1(A *);
void f2(struct B *);

struct B {
	A* a;
};

void f3(A *);
void f4(struct B *);

/*

type B as B_
type A as B

declare sub f1(byval as A ptr)
declare sub f2(byval as B ptr)

type B_
	a as A ptr
end type

declare sub f3(byval as A ptr)
declare sub f4(byval as B ptr)

*/
