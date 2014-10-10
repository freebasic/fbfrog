#ifdef _WIN32
	#define A1 055
	#define A2 055
	#define A3 055

	#define B1 55
	#define B2 55
	#define B3 55

	#define C1 0x55
	#define C2 0x55
	#define C3 0x55
#else
	#define A1 055
	#define A2 55
	#define A3 0x55

	#define B1 055
	#define B2 55
	#define B3 0x55

	#define C1 055
	#define C2 55
	#define C3 0x55
#endif
