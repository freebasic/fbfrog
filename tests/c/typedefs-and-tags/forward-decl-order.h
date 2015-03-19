// Since fbfrog now inserts forward declarations right above the first
// declarations that are using them, the order in which they are added only
// matters if there are multiple forward declarations that have to be added
// for a single declaration.

struct UDT {
	struct A *aa;
	struct B *bb;
};

void f(struct C *cc, struct D *dd);

struct A { int i; };
struct B { int i; };
struct C { int i; };
struct D { int i; };
