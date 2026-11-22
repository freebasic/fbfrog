#pragma once

' These #defines use types which aren't declared yet. fbfrog should still be
' able to detect it though because they are declared later.
#define X1 cast(A, 0)
#define X2 cast(B, 0)
#define X3 cast(C, 0)
type A as long
' @ignore
type B as long

type C
	i as long
end type
