struct UDT1 {
	struct UDT2 {
		int a;
	}; // gcc: warning: declaration does not declare anything
};
// sizeof(UDT1) = 0, UDT2 does not become a field

struct UDT3 {
	// The "const" has no effect
	const struct UDT4 {
		int a;
	}; // gcc: warning: declaration does not declare anything
};
