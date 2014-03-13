extern "C"

'' @fbfrog -whitespace -nonamefixup

type UDT
end type

union U
end union

type T
	a as long
end type

type T
	a as long
end type

dim shared __typedef_structs as long

type UDT
	a as long
end type
type A as UDT
type A
	a as long
end type

type UDT
	a as long
end type
type A as UDT

type A
	a as long
end type

type UDT
	a as long
end type
type A as UDT
type B as UDT
type C as UDT ptr
type D as function() as UDT

type A
	a as long
end type
type PA as A ptr

union U
	a as long
end union
type UU as U
union UU
	a as long
end union

type T
	union
		type
			a as long
			b as long
		end type
		c as long
	end union
end type

union Nested
	a as long
	b as long
	type
		c as long
		union
			d as long
			e as long
		end union
		f as long
	end type
	g as long
	type
		h as long
	end type
	type
		union
			type
				union
				end union
			end type
		end union
	end type
end union

type A  '' Anonymous, will be given a temp id, requiring AST fix up later
	i as long
end type
type B as A
type C as A
type D as A ptr
type E as A ptr ptr
type F as function() as A

'' However, B shouldn't be typedeffed to A here
type B
	i as long
end type
type A as B ptr
type B
	i as long
end type
type A as B ptr ptr
type B
	i as long
end type
type A as function() as B

end extern
