struct UDT {
};

union U {
};

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

// Anonymous, will be given a temp id, requiring AST fix up later
typedef struct { int i; } A, B, C, *D, **E, (*F)(void);

// However, B shouldn't be typedeffed to A here
typedef struct { int i; } *A, B;
typedef struct { int i; } **A, B;
typedef struct { int i; } (*A)(void), B;
