static int i;

#if MAJOR == 1
	static int major1;
#elif MAJOR == 2
	static int major2;
#elif MAJOR == 3
	static int major3;
#elif MAJOR == 4
	static int major4;
#elif MAJOR == 5
	static int major5;
#elif MAJOR == 6
	static int major6;
#else
	#error "unexpected MAJOR value"
#endif

#if MINOR == 0
	static int minor0;
#elif MINOR == 1
	static int minor1;
#elif MINOR == 2
	static int minor2;
#elif MINOR == 3
	static int minor3;
#elif MINOR == 99
	static int minor99;
#else
	#error "unexpected MINOR value"
#endif
