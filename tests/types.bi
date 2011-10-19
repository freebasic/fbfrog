dim shared as integer a
dim shared as uinteger a

dim shared as integer a
dim shared as integer a
dim shared as uinteger a

dim shared as long a
dim shared as long a
dim shared as ulong a

dim shared as long a
dim shared as long a
dim shared as ulong a

dim shared as longint a
dim shared as longint a
dim shared as ulongint a

dim shared as longint a
dim shared as longint a
dim shared as ulongint a

dim shared as single a
dim shared as double a

dim shared as __int8 a
dim shared as __int32 a
dim shared as __int64 a

 dim shared as int8_t a
dim shared as uint8_t a
 dim shared as int32_t a
dim shared as uint32_t a
 dim shared as int64_t a
dim shared as uint64_t a

dim shared as A a
dim shared as A a
dim shared as A a
dim shared as A a

'' Pointers

dim shared as integer ptr a
dim shared as integer ptr ptr ptr ptr a
dim shared as integer ptr a : dim shared as integer ptr ptr b

'' Split up based on different ptrcount
dim shared as integer a : dim shared as integer ptr b : dim shared as integer ptr ptr c
dim shared as integer ptr ptr a : dim shared as integer ptr b : dim shared as integer c
dim shared as integer a : dim shared as integer ptr b : dim shared as integer c
dim shared as integer ptr a : dim shared as integer b : dim shared as integer ptr c
dim shared as integer ptr a, b : dim shared as integer c
dim shared as integer a : dim shared as integer ptr b, c

'' CONSTness
dim shared as const integer a
dim shared as const integer a
dim shared as const integer a
dim shared as integer const ptr a
dim shared as const integer const ptr a
dim shared as const integer const ptr ptr ptr const ptr a

'' Split up based on different ptr const mask
dim shared as integer const ptr a : dim shared as integer ptr b : dim shared as integer ptr ptr const ptr ptr c
