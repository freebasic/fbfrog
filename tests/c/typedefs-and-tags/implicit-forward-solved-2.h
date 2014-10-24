struct A {
	struct B *b;
};

struct B {
	struct A *a;
};

/*

type B as B_

type A
    b as B ptr
end type

type B_
    a as A ptr
end type

*/
