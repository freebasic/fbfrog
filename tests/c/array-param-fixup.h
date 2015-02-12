void f(int i[]);
extern void (*p)(int i[]);
void f(void (*p)(int i[]));

struct UDT {
	void f(int i[]);
	void (*p)(int i[]);
};

#define A (void (*)(int i[]))0

typedef struct C { int i; } C;
typedef C D[10];
struct UDT2 {
	void (*p)(const D x);
};
