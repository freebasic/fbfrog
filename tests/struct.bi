'' type T : ... : end type

'' type as T A
type T '' (Both ids might be needed)
	a as long
end type
type A as T

'' Anonymous struct typedef
type __fbfrog_AnonStruct1 '' type TT : ... : end type
	a as long
end type
type TT as __fbfrog_AnonStruct1

type __fbfrog_AnonStruct2 '' Anonymous struct typedef triggering the fake id insertion
	a as long
end type
type A as __fbfrog_AnonStruct2
type PA as __fbfrog_AnonStruct2 ptr

'' type T : ... : end type
type T '' type as T A, B : type as T ptr C : type as function() as T D
	a as long
end type
type A as T
type B as T
type C as T ptr
type D as function( ) as T

'' type T : ... : end type
type T '' (also, any places using <struct T> will become just <T>, so they work ok)
	a as long
end type

type __fbfrog_AnonStruct3
	a as long
end type
type T as __fbfrog_AnonStruct3

type T
	a as long
end type

''*
'' * Unions
'' 
type U
	a as long
	b as long
	type
		c as long
		type
			d as long
			e as long
		end type
		f as long
	end type
	g as long
	type
		h as long
	end type
	type
		type
			type
				type
				end type
			end type
		end type
	end type
end type

'' union U : ... : end union

'' type UU as U
type U '' (Both ids might be needed)
	a as long
end type
type UU as U

type __fbfrog_AnonStruct4 '' union UU : ... : end union
	a as long
end type
type UU as __fbfrog_AnonStruct4

type __fbfrog_AnonStruct5
	type
		type
			a as long
			b as long
		end type
		c as long
	end type
end type
type T as __fbfrog_AnonStruct5

''*
'' * Enums
'' 

'' Anonymous enum
'' TODO: unknown construct (sorry)
enum {
	A = 0,               
	B, C = (1 << 4),     
	D,                   
	E = CALC(1,2,3),
	F
};

'' enum E : ... : end enum
'' TODO: unknown construct (sorry)
enum E {
	A,
'' TODO: unknown PP directive (sorry)
#if 1
	'' TODO: unknown construct (sorry)
B,
#endif
'' TODO: unknown construct (sorry)
C
};

'' TODO: unknown construct (sorry)
enum {
	A =
	1
	+
	CALC(2,3)
	,
	B
};

'' enum E : ... : end enum

'' type EE as E
'' TODO: unknown construct (sorry)
typedef enum E { A, B } EE;

'' Extra comma
'' TODO: unknown construct (sorry)
enum E { A, B, };
enum E{A,B,};
enum E { A, B,
};

'' TODO: unknown construct (sorry)
typedef enum {
	A,
	B,
	C
} EE;

'' TODO: unknown construct (sorry)
typedef enum E {
A, B} EE;

'' TODO: unknown construct (sorry)
typedef enum E {A,
B} EE;

'' TODO: unknown construct (sorry)
typedef enum E {A, B
} EE;

''*
'' * Extern blocks
'' 
'' TODO: unknown construct (sorry)
extern "C" {
}

'' TODO: unknown construct (sorry)
extern "C"{}

'' TODO: unknown construct (sorry)
extern "C" { }

'' TODO: unknown construct (sorry)
extern
"C"{}

'' TODO: unknown construct (sorry)
extern
"C"
{
}

#ifdef __cplusplus
'' TODO: unknown construct (sorry)
extern "C" {
#endif

'' foo 
#ifdef __cplusplus
'' TODO: unknown construct (sorry)
}
#endif

type UDT '' fields
	i as long
	j as ulongint
	k as ulong
	a as double
	b as double
	c as double

	a as long ptr ptr
	a as long ptr
	a as long
	a as long
	a as long ptr ptr
	a as long ptr ptr
	a as long ptr
	a as long
	a as long
	a as long ptr ptr
	a as long ptr ptr
	a as long

	y as T ptr ptr ptr ptr
	a as T ptr
	a as T ptr ptr ptr ptr
	a as T

	p as function( byval as long ptr ) as long ptr
	declare function f( byval as long, byval as long ) as long
	declare sub proc( )

	a as long
	'' TODO: unknown PP directive (sorry)
#if 1
		b as long
	#endif
	c as long
end type
