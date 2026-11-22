#pragma once

extern "C"

' @fbfrog -v
' Unknown constructs nested inside compound blocks

type A
	'' TODO: x;
end type

union B
	'' TODO: x;
end union

enum C
	'' TODO: x;
end enum

private sub f()
	'' TODO: x=;
end sub

end extern
