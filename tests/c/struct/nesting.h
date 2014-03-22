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
