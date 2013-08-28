struct T {
	int a;
};

struct T { int a; };

static int __typedef_structs;

typedef struct UDT { int a; } A;
typedef struct     { int a; } A;

typedef struct UDT {
	int a;
} A;

typedef struct {
	int a;
} A;

typedef struct UDT {
	int a;
} A, B, *C, (*D)(void);

typedef struct {
	int a;
} A, *PA;

typedef union U { int a; } UU;
typedef union   { int a; } UU;

typedef struct { union { struct { int a; int b; }; int c; }; } T;

union Nested {
	int a;
	int b;
	struct {
		int c;
		union {
			int d;
			int e;
		};
		int f;
	};
	int g;
	struct { int h; };
	struct {
		union {
			struct {
				union {
				};
			};
		};
	};
};
