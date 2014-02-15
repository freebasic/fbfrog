#define D1
struct UDT1 {
	int a;
	#define D2
};

struct UDT2 {
	int a;
	#define D3
	#define D4
	#define D5
	#define D6
	#define D7
};

struct UDT3 {
	int a;
	union {
		#define D8
		#define D9
		int b;
		#define D10
		#define D11
		#define D12
	};
};
