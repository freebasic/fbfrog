extern "C"

type __freebasic_tagid_AAA
	i as long
end type

dim shared p as AAA ptr

declare sub f(byval p as AAA ptr)

type BBB
	aaa as AAA
end type

type CCC as AAA

end extern
