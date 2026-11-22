#pragma once

extern "C"

' @fbfrog -rename Unrelated Unrelated_
' A forward declaration will be added for "UDT" -- and it shouldn't be added
' to the renamelist just because there is some -rename option.
type UDT as UDT_
extern p as UDT ptr

type UDT_
	dummy as long
end type

end extern
