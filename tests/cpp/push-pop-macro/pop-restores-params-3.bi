#pragma once

#define A(x)
' Temporarily redefined with different parameters
' (not even with different body, just different parameters!)
#undef A
#define A(x, y)

dim shared x as long ' Using A should require 2 arguments now
dim shared y as long ' Using A should require 1 argument now
