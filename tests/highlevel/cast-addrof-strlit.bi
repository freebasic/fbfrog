#pragma once

type myStr as byte ptr
#define A cptr(zstring ptr, @"foo")
#define B cast(myStr, @"foo")
#define C cptr(long ptr, @"123")
#define D cptr(zstring ptr, @"foo")
#define E cptr(zstring ptr, @"a" "b")
