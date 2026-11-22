#pragma once

extern "C"

' Illegal C, but still the situation could happen in fbfrog's AST after
' highlevel transformations, and FB supports it...
type T as T_
extern p as T ptr
type T_ as long

end extern
