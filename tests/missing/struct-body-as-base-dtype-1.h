// @fail

struct UDT {
	// Both a and b should be CONST
	struct { int a; } const a, b;
};
