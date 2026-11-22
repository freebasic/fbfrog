#pragma once

extern "C"

' @fbfrog -dropprocbody add2

private function add1(byval i as long) as long
	add1(i)
	return i + 1
end function

declare function add2(byval i as long) as long

end extern
