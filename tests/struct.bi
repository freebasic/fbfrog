'' type T : ... : end type
'' type TT as T
'' (Both ids might be needed)
type T
	as integer a
	as double x
end type : type TT as T

'' type TT : ... : end type
type TT
	as integer a
	as double x
end type

'' type T : ... : end type
'' (also, any places using <struct T> will become just <T>, so they work ok)
type T
	as integer i
#if 1
	as ulongint j
#endif
	as uinteger k
	as double a,b,c
	as T2 ptr ptr ptr ptr y
end type

type T : as integer a : end type
