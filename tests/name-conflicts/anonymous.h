void f(int);

enum {
	A, B
};

struct UDT1 {
	int a;
	union {
		int b;
		struct {
			int c;
		};
	};
};

union UDT2 {
	int a;
	struct {
		int b;
		union {
			int c;
		};
	};
};
