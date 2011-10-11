'' type T2 : ... : end type
'' type TT2 as T2
'' (Both ids might be needed)
type T2 
	as integer a
	as double x
end type : type TT2 as T2

'' type TT3 : ... : end type
type TT3
	as integer a
	as double x
end type

'' type T1 : ... : end type
'' (also, any places using <struct T1> will become just <T1>, so they work ok)
type T1 
	as integer i
	as ulongint j
	as uinteger k
	as double a,b,c
	as T2 ptr ptr ptr ptr y
end type

type T3 : as integer a : end type
