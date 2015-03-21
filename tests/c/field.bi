#pragma once

extern "C"

type UDT
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
	p as function(byval as long ptr) as long ptr
	declare function f(byval as long, byval as long) as long
	declare sub proc()
	a as long
	b as long
	c as long
	a : 1 as long
	a : 3 as long
	a : 27 as long
	a : 1 + (5 * 4) as long
	a : 1 as long
	b : 1 as long
	a as long
	a as long ptr ptr
	a as long
	b as long
	a as long ptr
	b as long
	c as long ptr
	d as long ptr ptr ptr
	a(0 to 19) as long
	a(0 to 1, 0 to 2) as long
	p(0 to 39) as sub()
	p(0 to 1, 0 to 2) as sub()

	declare sub f()
	declare function f() as long
	declare sub f()
	declare function f() as UDT ptr ptr
	declare sub f(byval a as long ptr, byval b as long ptr ptr ptr)

	a as sub()
	a as function(byval as long) as long
	a as function(byval a as long) as long
	b as function(byval a as long) as long
	c as long
	a as long
	b as function(byval a as long) as long
	c as long
	d as function(byval a as long) as long
	a as function(byval as long) as long ptr ptr
	declare sub f(byval a as sub())
	declare sub f(byval as sub())
	a as sub(byval a as sub())
	p as function(byval as function(byval as long ptr ptr ptr) as long ptr ptr ptr) as long ptr ptr ptr
end type

end extern
