struct B {
};

void f1(struct B *);

typedef struct B A;

void f2(A *);

/*

type B
end type

declare sub f1(byval as B ptr)

type A as B

declare sub f2(byval as A ptr)

*/
