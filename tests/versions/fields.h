// @fbfrog -version 1 -define A -version 2

struct UDT1 {
	int field1;
	int field2;
};

struct UDT2 {
	int field1;
	#ifdef A
		int field2;
	#else
		int field3;
	#endif
	int field4;
};
