'' type T : ... : end type
'' type as T A
'' (Both ids might be needed)
type T
	as integer a
end type : type as T A

'' Anonymous struct typedef
'' type TT : ... : end type
type TT
	as integer a
end type

'' Anonymous struct typedef triggering the fake id insertion
type /' TODO: faked id '/ FAKE
	as integer a
end type : type as FAKE A : type as FAKE ptr PA

'' type T : ... : end type
'' type as T A, B : type as T ptr C : type as function() as T D
type T
	as integer a
end type : type as T A, B : type as T ptr C : type as function() as T D

'' type T : ... : end type
'' (also, any places using <struct T> will become just <T>, so they work ok)
type T
	as integer a
end type

type T : as integer a : end type

type T : as integer a : end type



/'*
 * Unions
 '/

union U
	as integer a
	as integer b
	type
		as integer c
		union
			as integer d
			as integer e
		end union
		as integer f
	end type
	as integer g
	type : as integer h : end type
	type
		union
			type
				union
				end union
			end type
		end union
	end type
end union

'' union U : ... : end union
'' type UU as U
'' (Both ids might be needed)
union U : as integer a : end union : type as U UU

'' union UU : ... : end union
union UU : as integer a : end union

type T : union : type : as integer a : as integer b : end type : as integer c : end union : end type



/'*
 * Enums
 '/

'' Anonymous enum
'' enum : ... : end enum
enum
	A = 0               /' This is A '/
	B, C = (1 shl 4)     /' This is B and C '/
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



/'*
 * Extern blocks
 '/

extern "C" 
end extern

extern "C" : end extern

extern "C" : end extern

extern _
"C" : end extern

extern _
"C" _

end extern

#ifdef __cplusplus
extern "C" 
#endif

/' foo '/

#ifdef __cplusplus
end extern
#endif
