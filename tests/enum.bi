enum
	A = &o               /' This is A '/
	B, C = (1 shl 4)     /' This is B and C '/
	D                   /' This is D '/
	E = CALC(1,2,3)
	F
end enum

'' enum E2 : ... : end enum
'' type EE2 as E2
'' (Both ids might be needed)
enum E2 : A, B end enum : type EE2 as E2

'' enum EE3 : ... : end enum
enum EE3
	A
	B
	C
end enum

'' enum E1 : ... : end enum
'' (also, any places using <enum E1> will become just <E1>, so they work ok)
enum E1
	A
#if 1
	B
#endif
	C
end enum

/' expression split across multiple lines, but should be fixed up with _ '/
enum
	A = _
	1 _
	+ _
	CALC(2,3) _
	
	B
end enum

enum
#if 1
	A    /' next token is '#' instead of ',' or '}', preventing the translation '/
#endif
end enum

/' Cannot have #directives mixed into the expression in FB '/
enum
	A =
	1
#if 1
	+
	2
#endif
	,
	B
end enum
