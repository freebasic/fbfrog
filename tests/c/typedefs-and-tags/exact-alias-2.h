struct A {
};

typedef struct A A;

void f1(struct A *);
void f2(A *);

/*

type A
end type

declare sub f1(byval as A ptr)
declare sub f2(byval as A ptr)

*/
