#pragma once

'' The following symbols have been renamed:
''     typedef A1 => A2

extern "C"

' @fbfrog -fixunsizedarrays -renametypedef A1 A2
' Internal renames due to the unsized array fixups shouldn't appear in the
' rename list.
type A2 as long
#define array1(i) ((@__array1)[i])
extern __array1 alias "array1" as long
extern __string1 alias "string1" as byte
#define string1 (*cptr(zstring ptr, @__string1))

end extern
