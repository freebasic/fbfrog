#pragma once

extern "C"

private function a(byval i as long) as long
	return i * i
end function

#define b(i) clng((i) * (i))

end extern
