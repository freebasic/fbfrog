#pragma once

extern "C"

type myubyte as ubyte
extern s1(0 to 9) as myubyte
extern s2(0 to 9) as myubyte
declare sub f1(byval p1 as zstring ptr, byval p2 as ubyte ptr)
declare sub f2(byval p3 as byte ptr, byval p4 as zstring ptr)
declare sub f3(byval p5 as myubyte ptr)
extern s3(0 to 9) as zstring

end extern
