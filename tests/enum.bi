enum
	A = &o               /' This is A '/
	B, C = (1 shl 4)     /' This is B and C '/
	D                   /' This is D '/
	E = CALC(1,2,3)
	F
end enum

'' enum E : ... : end enum
'' type EE as E
'' (Both ids might be needed)
enum E : A, B : end enum : type EE as E

enum E : A, B, : end enum : type EE as E

enum E
A, B : end enum : type EE as E

enum E : A
B : end enum : type EE as E

enum E : A, B
end enum : type EE as E


'' enum EE : ... : end enum
enum EE
	A
	B
	C
end enum

'' enum E : ... : end enum
'' (also, any places using <enum E> will become just <E>, so they work ok)
enum E
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
