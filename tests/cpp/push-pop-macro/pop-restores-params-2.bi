#pragma once

#define A(x)
' Temporarily redefined without parameter
' (not even with different body, just without the parameter!)
#undef A
#define A

dim shared x as long ' A without arguments should be expanded now
dim shared y as long = A ' A without arguments shouldn't be expanded
