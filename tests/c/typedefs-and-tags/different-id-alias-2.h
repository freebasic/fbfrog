struct B {
};

typedef struct B A;

void f1(struct B *);
void f2(A *);

/*

type B
end type

type A as B

declare sub f1(byval as B ptr)
declare sub f2(byval as A ptr)

*/
