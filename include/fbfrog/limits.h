#ifdef __linux__
	#ifdef __x86_64__
		#define __WORDSIZE 64
	#else
		#define __WORDSIZE 32
	#endif
#endif

#define CHAR_BIT 8
#define CHAR_MIN SCHAR_MIN
#define CHAR_MAX SCHAR_MAX
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127
#define UCHAR_MAX 255
#define SHRT_MIN (-32768)
#define SHRT_MAX 32767
#define USHRT_MAX 65535
#define INT_MIN (-INT_MAX - 1)
#define INT_MAX __INT_MAX__
#define UINT_MAX __UINT32_MAX__
#define LONG_MIN (-LONG_MAX - 1)
#define LONG_MAX __LONG_MAX__
#ifdef __LP64__
	#define ULONG_MAX __UINT64_MAX__
#else
	#define ULONG_MAX __UINT32_MAX__
#endif
#define LLONG_MIN (-LLONG_MAX - 1)
#define LLONG_MAX __INT64_MAX__
#define ULLONG_MAX __UINT64_MAX__
