#pragma once

extern "C"

' @ignore
extern preinclude1 as long
#define DECL(x) x
' @fbfrog -incdir <dir> -include preinclude-1.h
extern foo as long

end extern
