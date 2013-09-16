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
type __fbfrog_AnonStruct1
	a as long
end type
type A as __fbfrog_AnonStruct1

type UDT
	a as long
end type
type A as UDT

type __fbfrog_AnonStruct2
	a as long
end type
type A as __fbfrog_AnonStruct2

type UDT
	a as long
end type
type A as UDT
type B as UDT
type C as UDT ptr
type D as function() as UDT

type __fbfrog_AnonStruct3
	a as long
end type
type A as __fbfrog_AnonStruct3
type PA as __fbfrog_AnonStruct3 ptr

union U
	a as long
end union
type UU as U
union __fbfrog_AnonStruct4
	a as long
end union
type UU as __fbfrog_AnonStruct4

type __fbfrog_AnonStruct5
	union
		type
			a as long
			b as long
		end type
		c as long
	end union
end type
type T as __fbfrog_AnonStruct5

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
