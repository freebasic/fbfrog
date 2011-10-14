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
