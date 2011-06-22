#define NULL 0
#define FALSE 0
#define TRUE (-1)

declare sub oops(byref message as string)
declare sub xoops(byref message as string)
declare function xallocate(byval as ulong) as any ptr
declare function xcallocate(byval as ulong) as any ptr
declare function xreallocate(byval as any ptr, byval as ulong) as any ptr
declare function path_strip_ext(byref path as string) as string
