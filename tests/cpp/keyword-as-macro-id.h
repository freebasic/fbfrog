#define int char
	extern int i;
#undef int

#define define int
	extern define i;
#undef define

#define m1(int, define, defined) int define defined
	m1(void, f, (void));
#undef m1

#define int
	#ifdef int
		void ok(void);
	#else
		void wrong(void);
	#endif
	#ifndef int
		void wrong(void);
	#else
		void ok(void);
	#endif
	#if defined int
		void ok(void);
	#else
		void wrong(void);
	#endif
	#if defined(int)
		void ok(void);
	#else
		void wrong(void);
	#endif
	#define defined_int_1 defined int
		#if defined_int_1
			void ok(void);
		#else
			void wrong(void);
		#endif
	#undef defined_int_1
	#define defined_int_2 defined(int)
		#if defined_int_2
			void ok(void);
		#else
			void wrong(void);
		#endif
	#undef defined_int_2
#undef int

#ifdef long
	void wrong(void);
#else
	void ok(void);
#endif
#ifndef long
	void ok(void);
#else
	void wrong(void);
#endif
#if defined long
	void wrong(void);
#else
	void ok(void);
#endif
#if defined(long)
	void wrong(void);
#else
	void ok(void);
#endif
#define defined_long_1 defined long
	#if defined_long_1
		void wrong(void);
	#else
		void ok(void);
	#endif
#undef defined_long_1
#define defined_long_2 defined(long)
	#if defined_long_2
		void wrong(void);
	#else
		void ok(void);
	#endif
#undef defined_long_2

#define define
	#ifdef define
		void ok(void);
	#else
		void wrong(void);
	#endif
	#ifndef define
		void wrong(void);
	#else
		void ok(void);
	#endif
	#if defined define
		void ok(void);
	#else
		void wrong(void);
	#endif
	#if defined(define)
		void ok(void);
	#else
		void wrong(void);
	#endif
#undef define

#ifdef define
	void wrong(void);
#else
	void ok(void);
#endif
#ifndef define
	void ok(void);
#else
	void wrong(void);
#endif
#if defined define
	void wrong(void);
#else
	void ok(void);
#endif
#if defined(define)
	void wrong(void);
#else
	void ok(void);
#endif

#define __attribute__
#define __restrict
#define __restrict__
#define auto
#define break
#define case
#define char
#define const
#define continue
#define default
#define define
#define do
#define double
#define elif
#define else
#define endif
#define enum
#define error
#define extern
#define float
#define for
#define goto
#define if
#define ifdef
#define ifndef
#define include
#define inline
#define int
#define long
#define pragma
#define register
#define restrict
#define return
#define short
#define signed
#define sizeof
#define static
#define struct
#define switch
#define typedef
#define undef
#define union
#define unsigned
#define void
#define volatile
#define warning
#define while
