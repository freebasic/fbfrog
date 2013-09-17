struct UDT {
	int field1;
	int field2;
};

struct UDT {
	int field1;
	#ifdef A
		int field2;
	#else
		int field3;
	#endif
	int field4;
};
