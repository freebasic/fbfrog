type T
	as integer i
	as ulongint j
	as uinteger k
	as double a,b,c

	as integer ptr ptr a : as integer ptr a : as integer a
	as integer a : as integer ptr ptr a, a
	as integer ptr a : as integer a, a : as integer ptr ptr a, a : as integer a

	as T ptr ptr ptr ptr y
	as T ptr a : as T ptr ptr ptr ptr a : as T a

	as function(byval as integer ptr ) as integer ptr p

	as integer a
#if 1
	as integer b
#endif
	as integer c
end type
