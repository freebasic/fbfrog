#pragma once

extern "C"

#define M1 cptr(CHAR ptr, 0)
type CHAR as zstring
const M2 = cptr(zstring ptr, 0)
#define M3 cptr(FUNCTION ptr, 0)
const M4 = cptr(sub(), 0)

end extern
