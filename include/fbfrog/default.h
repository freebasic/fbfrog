#define __STDC__ 1
#define __STDC_VERSION__ 199901
#define __STDC_HOSTED__ 1
#define __STDC_IEC_559__ 1
#define __GNUC__ 4
#define __GNUC_MINOR__ 1111111
#define __GNUC_PATCHLEVEL__ 1111111
#define __GNUC_PREREQ(maj, min) ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
#define __clang__ 1
#define __clang_major__ 3
#define __clang_minor__ 1111111
#define __clang_patchlevel__ 1111111
#define __clang_version__ "fbfrog"
#define __llvm__ 1

// All Unix-likes including Cygwin
#if defined __FB_LINUX__ || \
    defined __FB_FREEBSD__ || \
    defined __FB_OPENBSD__ || \
    defined __FB_NETBSD__  || \
    defined __FB_DARWIN__ || \
    defined __FB_CYGWIN__
	#define unix 1
	#define __unix 1
	#define __unix__ 1
#endif

#if defined __FB_LINUX__ || \
    defined __FB_FREEBSD__ || \
    defined __FB_OPENBSD__ || \
    defined __FB_NETBSD__
	#define __ELF__ 1
#endif

#ifdef __FB_LINUX__
	#define linux 1
	#define __linux 1
	#define __linux__ 1
	#define __gnu_linux__ 1
#elif defined __FB_FREEBSD__
	#define __FreeBSD__ 10
	#define __FreeBSD_cc_version 1000001
#elif defined __FB_OPENBSD__
	#define __OpenBSD__ 1
#elif defined __FB_NETBSD__
	#define __NetBSD__ 1
#elif defined __FB_DARWIN__
	#define __APPLE__ 1
	#define __MACH__ 1
#elif defined __FB_WIN32__
	#define __MINGW32__ 1
	#define __MSVCRT__ 1
	#define WIN32 1
	#define _WIN32 1
	#define __WIN32 1
	#define __WIN32__ 1
	#define WINNT 1
	#define __WINNT 1
	#define __WINNT__ 1
	#ifdef __FB_64BIT__
		#define __MINGW64__ 1
		#define WIN64 1
		#define _WIN64 1
		#define __WIN64 1
		#define __WIN64__ 1
	#endif
#elif defined __FB_CYGWIN__
	#define __CYGWIN__ 1
	#ifndef __FB_64BIT__
		#define __CYGWIN32__ 1
	#endif
#elif defined __FB_DOS__
	#define DJGPP 2
	#define __DJGPP 2
	#define __DJGPP__ 2
	#define GO32 1
	#define __GO32 1
	#define __GO32__ 1
	#define MSDOS 1
	#define __MSDOS 1
	#define __MSDOS__ 1
#endif

// 64bit Unix-likes, including Cygwin (!)
#if defined __FB_64BIT__ && defined __unix__
	#define __LP64__ 1
	#define _LP64 1
#endif

#if defined __FB_ARM__ && defined __FB_64BIT__
	// arm64
	#define __aarch64__ 1
	#define __AARCH64_CMODEL_SMALL__ 1
	#define __AARCH64EL__ 1
	#define __ARM_64BIT_STATE 1
	#define __ARM_ARCH 8
	#define __ARM_ARCH_8A 1
	#define __ARM_ARCH_ISA_A64 1
	#define __ARM_ARCH_PROFILE 65
#elif defined __FB_ARM__
	// arm32
	#define __APCS_32__ 1
	#define __arm__ 1
	#define __ARM_32BIT_STATE 1
	#define __ARM_ARCH 7
	#define __ARM_ARCH_7A__ 1
	#define __ARM_ARCH_ISA_ARM 1
	#define __ARM_ARCH_ISA_THUMB 2
	#define __ARM_ARCH_PROFILE 65
	#define __ARM_EABI__ 1
	#define __ARMEL__ 1
	#define __ARM_FEATURE_CLZ 1
	#define __ARM_FEATURE_DSP 1
	#define __ARM_FEATURE_LDREX 15
	#define __ARM_FEATURE_QBIT 1
	#define __ARM_FEATURE_SAT 1
	#define __ARM_FEATURE_SIMD32 1
	#define __ARM_FEATURE_UNALIGNED 1
	#define __ARM_FP 12
	#define __ARM_NEON_FP 4
	#define __ARM_PCS_VFP 1
	#define __ARM_SIZEOF_MINIMAL_ENUM 4
	#define __ARM_SIZEOF_WCHAR_T 4
#elif defined __FB_64BIT__
	#define __x86_64 1
	#define __x86_64__ 1
	#define __amd64 1
	#define __amd64__ 1
	#define __k8 1
	#define __k8__ 1
	#define _M_X64
	#define _M_AMD64
	#define __code_model_small__ 1
	#define __MMX__ 1
	#define __SSE__ 1
	#define __SSE2__ 1
	#define __SSE2_MATH__ 1
	#define __SSE_MATH__ 1
#else
	#define i386 1
	#define __i386 1
	#define __i386__ 1
	#define __i486__ 1
	#define __i586 1
	#define __i586__ 1
	#define __pentium 1
	#define __pentium__ 1
	#define __i686 1
	#define __i686__ 1
	#define __pentiumpro 1
	#define __pentiumpro__ 1
	#define _M_IX86
	#define _X86_
	#define __X86__
	#define __code_model_32__ 1
#endif

#define __ORDER_BIG_ENDIAN__ 4321
#define __ORDER_LITTLE_ENDIAN__ 1234
#define __ORDER_PDP_ENDIAN__ 3412
#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#define __FLOAT_WORD_ORDER__ __ORDER_LITTLE_ENDIAN__
#define __LITTLE_ENDIAN__ 1

#define __CHAR_BIT__ 8
#define __SIZEOF_SHORT__ 2
#define __SIZEOF_INT__ 4
#ifdef __LP64__
	#define __SIZEOF_LONG__ 8
#else
	#define __SIZEOF_LONG__ 4
#endif
#ifdef __FB_64BIT__
	#define __SIZEOF_LONG_DOUBLE__ 16
	#define __SIZEOF_POINTER__ 8
	#define __SIZEOF_PTRDIFF_T__ 8
	#define __SIZEOF_SIZE_T__ 8
#else
	#ifdef __FB_ARM__
		#define __SIZEOF_LONG_DOUBLE__ 8
	#else
		#define __SIZEOF_LONG_DOUBLE__ 12
	#endif
	#define __SIZEOF_POINTER__ 4
	#define __SIZEOF_PTRDIFF_T__ 4
	#define __SIZEOF_SIZE_T__ 4
#endif
#define __SIZEOF_LONG_LONG__ 8
#define __SIZEOF_INT128__ 16
#define __SIZEOF_FLOAT__ 4
#define __SIZEOF_DOUBLE__ 8
#if defined __FB_ARM__ && !defined __FB_64BIT__
	#define __BIGGEST_ALIGNMENT__ 8
#else
	#define __BIGGEST_ALIGNMENT__ 16
#endif

#define __INT8_TYPE__ signed char
#define __INT8_MAX__ 127
#define __UINT8_TYPE__ unsigned char
#define __UINT8_MAX__ 255
#define __INT16_TYPE__ short int
#define __INT16_MAX__ 32767
#define __UINT16_TYPE__ short unsigned int
#define __UINT16_MAX__ 65535
#define __INT32_TYPE__ int
#define __INT32_MAX__ 2147483647
#define __UINT32_TYPE__ unsigned int
#define __UINT32_MAX__ 4294967295U
#define __INT64_TYPE__ long long int
#define __INT64_MAX__ 9223372036854775807LL
#define __UINT64_TYPE__ long long unsigned int
#define __UINT64_MAX__ 18446744073709551615ULL

#define __SCHAR_MAX__ __INT8_MAX__
#define __SHRT_MAX__ __INT16_MAX__
#define __INT_MAX__ __INT32_MAX__
#define __LONG_LONG_MAX__ __INT64_MAX__
#define __INTMAX_TYPE__ __INT64_TYPE__
#define __INTMAX_MAX__ __INT64_MAX__
#define __UINTMAX_TYPE__ __UINT64_TYPE__
#define __UINTMAX_MAX__ __UINT64_MAX__
#if __SIZEOF_LONG__ == 8
	#define __LONG_MAX__ __INT64_MAX__
#else
	#define __LONG_MAX__ __INT32_MAX__
#endif
#ifdef __FB_64BIT__
	#define __SIZE_TYPE__ __UINT64_TYPE__
	#define __SIZE_MAX__ __UINT64_MAX__
	#define __PTRDIFF_TYPE__ __INT64_TYPE__
	#define __PTRDIFF_MAX__ __INT64_MAX__
	#define __INTPTR_TYPE__ __INT64_TYPE__
	#define __INTPTR_MAX__ __INT64_MAX__
	#define __UINTPTR_TYPE__ __UINT64_TYPE__
	#define __UINTPTR_MAX__ __UINT64_MAX__
#else
	#define __SIZE_TYPE__ __UINT32_TYPE__
	#define __SIZE_MAX__ __UINT32_MAX__
	#define __PTRDIFF_TYPE__ __INT32_TYPE__
	#define __PTRDIFF_MAX__ __INT32_MAX__
	#define __INTPTR_TYPE__ __INT32_TYPE__
	#define __INTPTR_MAX__ __INT32_MAX__
	#define __UINTPTR_TYPE__ __UINT32_TYPE__
	#define __UINTPTR_MAX__ __UINT32_MAX__
#endif
#ifdef __FB_DOS__
	// wchar_t = uint16, wint_t = int32
	#define __SIZEOF_WCHAR_T__ 2
	#define __WCHAR_TYPE__ __UINT16_TYPE__
	#define __WCHAR_MIN__ 0
	#define __WCHAR_MAX__ __UINT16_MAX__
	#define __SIZEOF_WINT_T__ 4
	#define __WINT_TYPE__ __INT32_TYPE__
	#define __WINT_MIN__ (-__WINT_MAX__ - 1)
	#define __WINT_MAX__ __INT32_MAX__
#elif defined __FB_CYGWIN__
	// wchar_t = uint16, wint_t = uint32
	#define __SIZEOF_WCHAR_T__ 2
	#define __WCHAR_TYPE__ __UINT16_TYPE__
	#define __WCHAR_MIN__ 0u
	#define __WCHAR_MAX__ __UINT16_MAX__
	#define __SIZEOF_WINT_T__ 4
	#define __WINT_TYPE__ __UINT32_TYPE__
	#define __WINT_MIN__ 0u
	#define __WINT_MAX__ __UINT32_MAX__
#elif defined __FB_WIN32__
	// wchar_t = uint16, wint_t = uint16
	#define __SIZEOF_WCHAR_T__ 2
	#define __WCHAR_TYPE__ __UINT16_TYPE__
	#define __WCHAR_MIN__ 0
	#define __WCHAR_MAX__ __UINT16_MAX__
	#define __SIZEOF_WINT_T__ 2
	#define __WINT_TYPE__ __UINT16_TYPE__
	#define __WINT_MIN__ 0
	#define __WINT_MAX__ __UINT16_MAX__
#else
	#ifdef __FB_ARM__
		// wchar_t = uint32, wint_t = uint32
		#define __SIZEOF_WCHAR_T__ 4
		#define __WCHAR_TYPE__ __UINT32_TYPE__
		#define __WCHAR_MIN__ 0u
		#define __WCHAR_MAX__ __UINT32_MAX__
		#define __SIZEOF_WINT_T__ 4
		#define __WINT_TYPE__ __UINT32_TYPE__
		#define __WINT_MIN__ 0u
		#define __WINT_MAX__ __UINT32_MAX__
	#else
		#if defined __FB_FREEBSD__ || \
		    defined __FB_OPENBSD__ || \
		    defined __FB_NETBSD__ || \
		    defined __FB_DARWIN__
			// wchar_t = int32, wint_t = int32
			#define __SIZEOF_WCHAR_T__ 4
			#define __WCHAR_TYPE__ __INT32_TYPE__
			#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)
			#define __WCHAR_MAX__ __INT32_MAX__
			#define __SIZEOF_WINT_T__ 4
			#define __WINT_TYPE__ __INT32_TYPE__
			#define __WINT_MIN__ (-__WINT_MAX__ - 1)
			#define __WINT_MAX__ __INT32_MAX__
		#elif defined __FB_LINUX__
			// wchar_t = int32, wint_t = uint32
			#define __SIZEOF_WCHAR_T__ 4
			#define __WCHAR_TYPE__ __INT32_TYPE__
			#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)
			#define __WCHAR_MAX__ __INT32_MAX__
			#define __SIZEOF_WINT_T__ 4
			#define __WINT_TYPE__ __UINT32_TYPE__
			#define __WINT_MIN__ 0u
			#define __WINT_MAX__ __UINT32_MAX__
		#endif
	#endif
#endif
#define __SIG_ATOMIC_TYPE__ __INT32_TYPE__
#define __SIG_ATOMIC_MIN__ (-__SIG_ATOMIC_MAX__ - 1)
#define __SIG_ATOMIC_MAX__ __INT32_MAX__
#define __FLT_MIN__ 1.17549435082228750797e-38F
#define __FLT_MAX__ 3.40282346638528859812e+38F
#define __DBL_MIN__ ((double)2.22507385850720138309e-308L)
#define __DBL_MAX__ ((double)1.79769313486231570815e+308L)
#define __LDBL_MIN__ 3.36210314311209350626e-4932L
#define __LDBL_MAX__ 1.18973149535723176502e+4932L
#define __CHAR16_TYPE__ __UINT16_TYPE__
#define __CHAR32_TYPE__ __UINT32_TYPE__

#if defined __FB_LINUX__ || \
    defined __FB_WIN32__ || \
    defined __FB_CYGWIN__ || \
    defined __FB_DOS__
	#define __INT_FAST8_TYPE__ __INT8_TYPE__
	#define __INT_FAST8_MAX__ __INT8_MAX__
	#define __UINT_FAST8_TYPE__ __UINT8_TYPE__
	#define __UINT_FAST8_MAX__ __UINT8_MAX__
	#if defined __FB_64BIT__ && \
	    (defined __FB_LINUX__ || \
	     defined __FB_CYGWIN__)
		#define __INT_FAST16_TYPE__ __INT64_TYPE__
		#define __INT_FAST16_MAX__ __INT64_MAX__
		#define __UINT_FAST16_TYPE__ __UINT64_TYPE__
		#define __UINT_FAST16_MAX__ __UINT64_MAX__
		#define __INT_FAST32_TYPE__ __INT64_TYPE__
		#define __INT_FAST32_MAX__ __INT64_MAX__
		#define __UINT_FAST32_TYPE__ __UINT64_TYPE__
		#define __UINT_FAST32_MAX__ __UINT64_MAX__
		#define __INT_FAST64_TYPE__ __INT64_TYPE__
		#define __INT_FAST64_MAX__ __INT64_MAX__
		#define __UINT_FAST64_TYPE__ __UINT64_TYPE__
		#define __UINT_FAST64_MAX__ __UINT64_MAX__
	#else
		#if defined __FB_DOS__ || defined __FB_WIN32__
			#define __INT_FAST16_TYPE__ __INT16_TYPE__
			#define __INT_FAST16_MAX__ __INT16_MAX__
			#define __UINT_FAST16_TYPE__ __UINT16_TYPE__
			#define __UINT_FAST16_MAX__ __UINT16_MAX__
		#else
			#define __INT_FAST16_TYPE__ int
			#define __INT_FAST16_MAX__ __INT32_MAX__
			#define __UINT_FAST16_TYPE__ __UINT32_TYPE__
			#define __UINT_FAST16_MAX__ __UINT32_MAX__
		#endif
		#define __INT_FAST32_TYPE__ int
		#define __INT_FAST32_MAX__ __INT32_MAX__
		#define __UINT_FAST32_TYPE__ __UINT32_TYPE__
		#define __UINT_FAST32_MAX__ __UINT32_MAX__
		#define __INT_FAST64_TYPE__ __INT64_TYPE__
		#define __INT_FAST64_MAX__ __INT64_MAX__
		#define __UINT_FAST64_TYPE__ __UINT64_TYPE__
		#define __UINT_FAST64_MAX__ __UINT64_MAX__
	#endif
	#define __INT_LEAST8_TYPE__ __INT8_TYPE__
	#define __INT_LEAST8_MAX__ __INT8_MAX__
	#define __UINT_LEAST8_TYPE__ __UINT8_TYPE__
	#define __UINT_LEAST8_MAX__ __UINT8_MAX__
	#define __INT_LEAST16_TYPE__ __INT16_TYPE__
	#define __INT_LEAST16_MAX__ __INT16_MAX__
	#define __UINT_LEAST16_TYPE__ __UINT16_TYPE__
	#define __UINT_LEAST16_MAX__ __UINT16_MAX__
	#define __INT_LEAST32_TYPE__ int
	#define __INT_LEAST32_MAX__ __INT32_MAX__
	#define __UINT_LEAST32_TYPE__ __UINT32_TYPE__
	#define __UINT_LEAST32_MAX__ __UINT32_MAX__
	#define __INT_LEAST64_TYPE__ __INT64_TYPE__
	#define __INT_LEAST64_MAX__ __INT64_MAX__
	#define __UINT_LEAST64_TYPE__ __UINT64_TYPE__
	#define __UINT_LEAST64_MAX__ __UINT64_MAX__
#endif

#define __INT8_C(c) c
#define __INT16_C(c) c
#define __INT32_C(c) c
#define __INT64_C(c) c ## LL
#define __INTMAX_C(c) c ## LL
#define __UINT8_C(c) c
#define __UINT16_C(c) c
#define __UINT32_C(c) c ## U
#define __UINT64_C(c) c ## ULL
#define __UINTMAX_C(c) c ## ULL

#if defined __FB_DOS__ || \
    defined __FB_WIN32__ || \
    defined __FB_CYGWIN__
	#define __USER_LABEL_PREFIX__ _
#else
	#define __USER_LABEL_PREFIX__
#endif

#ifdef __FB_WIN32__
	#define _INTEGRAL_MAX_BITS 64
	#define _REENTRANT 1
#endif

#if defined __FB_WIN32__ || defined __FB_CYGWIN__
	#define __cdecl __attribute__((__cdecl__))
	#define _cdecl __attribute__((__cdecl__))
	#define __declspec(x) __attribute__((x))
	#define __fastcall __attribute__((__fastcall__))
	#define _fastcall __attribute__((__fastcall__))
	#define __stdcall __attribute__((__stdcall__))
	#define _stdcall __attribute__((__stdcall__))
	#define __thiscall __attribute__((__thiscall__))
	#define _thiscall __attribute__((__thiscall__))
#endif

#define __extension__
#define register
#define __has_feature(x) 0
