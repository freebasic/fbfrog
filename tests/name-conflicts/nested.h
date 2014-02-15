#define GLOBALDEFINE1 1
void globalproc(void);
typedef int globaltype;

struct UDT1 {
	// shouldn't conflict with symbols from global scope, except defines
	int globaldefine1;
	int globaldefine2;
	int globalproc;
	int globaltype;

	// conflict with each-other
	int foo;
	int FOO;
	union {
		int Foo;
		int fOO;
	};

	// conflicts with FB keywords
	int as;
	int IF;
};

void f1(
	int globaldefine1,
	int globaldefine2,
	int globalproc,
	int globaltype,

	int foo,
	int FOO,

	int as,
	int IF
);

#define GLOBALDEFINE2 1

struct UDT2 {
	// shouldn't conflict with fields from UDT1
	int foo;
};

void f2(
	int foo
);
