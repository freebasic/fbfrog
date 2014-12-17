void scopeblock1(void) {
	struct UDT {
		struct {
			int i;
		} inner;
	};
}

#define scopeblock2 {			\
	struct UDT {			\
		struct {		\
			int i;		\
		} inner;		\
	};				\
}
