struct UDT1 {
	struct {
		int a;
	};
};

struct UDT2 {
	struct {
		int a;
		char b;
	} __attribute__((packed));
};
