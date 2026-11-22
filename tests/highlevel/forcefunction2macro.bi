#pragma once

extern "C"

' @fbfrog -forcefunction2macro b

private function a(byval i as long) as long
	return i * i
end function

#define b(i) clng((i) * (i))

end extern
