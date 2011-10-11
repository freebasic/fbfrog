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
union U : as integer a : end union : type UU as U

'' union UU : ... : end union
union UU : as integer a : end union

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

type T : union : type : as integer a : as integer b : end type : as integer c : end union : end type
