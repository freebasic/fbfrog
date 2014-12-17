#ifdef _WIN32
	#pragma pack(push, 8)
	struct UDT {
		int i;
	};
#else
	struct UDT {
		int i;
	};
#endif
