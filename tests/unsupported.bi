type T as long

enum
	#if 1
		'' TODO: unknown construct
		A
	#endif
end enum

enum
	'' TODO: unknown construct
	A =
 1
	#if 1
		'' TODO: unknown construct
		+
 2
	#endif
	'' TODO: unknown construct
	,
	B
end enum

type T
	'' TODO: unknown construct
	union {
  int a;
 } foo;
end type

'' TODO: unknown construct
typedef int MYINTARRAY[10];

'' TODO: unknown construct
void f(int myintarrayparam[10]);
