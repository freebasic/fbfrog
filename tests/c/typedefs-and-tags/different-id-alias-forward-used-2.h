typedef struct B A;

void f1(A *);
void f2(struct B *);

/*

type B as B_
type A as B

declare sub f1(byval as A ptr)
declare sub f2(byval as B ptr)

*/
