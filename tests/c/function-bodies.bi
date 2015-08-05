#pragma once

extern "C"

private sub f1()
end sub

private sub f2()
	dim i as long = 1
	printf(!"%i\n", 1)
end sub

private function f3(byval i as long) as long
	return (i shl 3) and &hFF
	return 0
end function

private sub f4()
	return
end sub

private function f(byval as long) as long
	f(0)
	return 0
end function

private function f() as long
	f()
end function

private sub f()
	f()
end sub

#define f(i) clng(i)

private function f(byval i as long) as long
	return i + i
end function

#define f(a, b) clng((a) + (b))

private function f(byval a as long, byval b as long) as long
	return (a + b) + b
end function

#define f(i) clng((i) * 4)
#define f(i) clng(sizeof(i))

end extern
