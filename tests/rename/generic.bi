#pragma once

'' The following symbols have been renamed:
''     enum E => E_
''     constant A => A_
''     typedef T => T_
''     constant B => B_
''     procedure f => f_
''     variable i => i_

extern "C"

' @fbfrog -rename E E_ -rename A A_ -rename B B_ -rename f f_ -rename p p_ -rename i i_ -rename T T_

enum E_
	A_ = 0
end enum

type T_ as E_
const B_ = A_
declare function f_ alias "f"(byval p_ as T_ = A_) as T_
extern i_ alias "i" as T_

end extern
