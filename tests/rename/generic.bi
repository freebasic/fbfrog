#pragma once

'' The following symbols have been renamed:
''     enum E => E_
''     constant A => A_
''     typedef T => T_
''     #define B => B_
''     procedure f => f_
''     variable i => i_

extern "C"

type E_ as long
enum
	A_ = 0
end enum

type T_ as E_
#define B_ A_
declare function f_ alias "f"(byval p_ as T_ = A_) as T_
extern i_ alias "i" as T_

end extern
