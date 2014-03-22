struct UDT1 {
	struct UDT2 {
		int a;
	}; // gcc: warning: declaration does not declare anything
};
// sizeof(UDT1) = 0, UDT2 does not become a field
