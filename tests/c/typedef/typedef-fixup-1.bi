#pragma once

extern "C"

' Anonymous, will be given a temp id, requiring AST fix up later

type A
	i as long
end type

type B as A
type C as A
type D as A ptr
type E as A ptr ptr
type F as function() as A

end extern
