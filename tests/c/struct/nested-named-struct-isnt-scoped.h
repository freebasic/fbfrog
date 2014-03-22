struct UDT1 {
	struct UDT2 {
		int a;
	};
};

// UDT2 is visible here, compiles fine with gcc
static struct UDT2 x;
