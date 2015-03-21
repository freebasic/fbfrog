#pragma once

extern "C"

dim shared __simple as long
declare sub f(byval a as long)
declare sub f(byval a as long, byval b as long)
declare sub f(byval a as long, byval b as long, byval c as long)
declare sub f(byval a as long ptr, byval b as long ptr ptr ptr)
dim shared __anonymous as long
declare sub f(byval as long)
declare sub f(byval as long ptr, byval as long, byval as long ptr ptr ptr, byval as long)
dim shared __initializers as long
declare sub f(byval i as long = 123)
declare sub f(byval a as long, byval b as long = 123)
declare sub f(byval a as long = 123, byval b as long)
declare sub f(byval a as long, byval b as long = 123, byval c as long)
declare sub f(byval a as long = 1, byval b as long = 2, byval c as long = 3)
declare sub f(byval p as sub(byval as long) = 0)
declare sub f(byval p as sub(byval i as long = 123) = 0)
dim shared __arrays as long
declare sub f(byval i as long ptr)
declare sub f(byval a as long, byval b as long ptr)
declare sub f(byval a as long ptr, byval b as long)
declare sub f(byval a as long, byval b as long ptr, byval c as long)
declare sub f(byval a as long ptr, byval b as long ptr, byval c as long ptr)
declare sub f(byval a as long ptr)
dim shared __nested_id as long
declare sub f(byval a as long)
declare sub f(byval a as long, byval b as long)
declare sub f(byval a as long ptr, byval b as long ptr ptr ptr, byval c as long ptr)
dim shared __anonymous_nested_id as long
declare sub f(byval as long ptr ptr ptr, byval as long ptr)
dim shared __no_params as long
declare sub f()
declare sub f()
dim shared __procptr_params as long
declare sub f(byval a as sub())
declare sub f(byval as sub())
declare sub f(byval a as sub(byval b as sub()))
declare sub f(byval as sub(byval as sub()))
dim shared __vararg as long
declare sub f(byval a as long, ...)
dim shared __functions as long
declare sub f(byval as sub())
declare sub f(byval as sub())
declare sub f(byval as sub())
declare sub f(byval as sub())
declare sub f(byval as sub())
declare sub f(byval as sub())
declare sub f(byval f as sub())

end extern
