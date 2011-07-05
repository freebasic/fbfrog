''#define ENABLE_STATS

#define NULL 0
#define FALSE 0
#define TRUE (-1)

#define xassert(test) _
	if ((test) = FALSE) then : _
		_xassertfail(#test, __FILE__, __FUNCTION__, __LINE__) : _
	end if
declare sub _xassertfail _
	( _
		byval test as zstring ptr, _
		byval filename as zstring ptr, _
		byval funcname as zstring ptr, _
		byval linenum as integer _
	)
declare sub oops(byref message as string)
declare sub xoops(byref message as string)
declare function xallocate(byval as ulong) as any ptr
declare function xcallocate(byval as ulong) as any ptr
declare function xreallocate(byval as any ptr, byval as ulong) as any ptr
declare function path_strip_ext(byref path as string) as string
