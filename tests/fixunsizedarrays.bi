#pragma once

'' The following symbols have been renamed:
''     variable string => string_

extern "C"

#define array1(i) ((@__array1)[i])
extern __array1 alias "array1" as long
extern array2(0 to 9) as long
#define array3(i) ((@__array3)[i])
dim shared __array3 as long
dim shared array4(0 to 9) as long
#define string_(i) ((@__string_)[i])
extern __string_ alias "string" as long

end extern
