'' Anonymous enum
'' enum : ... : end enum
enum
	A = 0               /' This is A '/
	B, C = (1 << 4)     /' This is B and C '/
	D                   /' This is D '/
	E = CALC(1,2,3)
	F
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

'' enum E : ... : end enum
'' type EE as E
'' (Both ids might be needed)
enum E : A, B : end enum : type as E EE


'' Extra comma
enum E : A, B : end enum
enum E : A,B : end enum
enum E : A, B
end enum

'' enum EE : ... : end enum
enum EE
	A
	B
	C
end enum

enum E
A, B : end enum : type as E EE

enum E : A
B : end enum : type as E EE

enum E : A, B
end enum : type as E EE
