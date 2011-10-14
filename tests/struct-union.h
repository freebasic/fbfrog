union U {
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

// union U : ... : end union
// type UU as U
// (Both ids might be needed)
typedef union U { int a; } UU;

// union UU : ... : end union
typedef union { int a; } UU;

typedef struct { union { struct { int a; int b; }; int c; }; } T;
