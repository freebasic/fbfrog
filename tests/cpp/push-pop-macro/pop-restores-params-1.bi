#pragma once

#define A
' Temporarily redefined with parameter
' (not even with different body, just the parameter!)
#undef A
#define A(x)

dim shared x as long = A ' A without arguments shouldn't be expanded anymore
dim shared y as long ' A without arguments should be expanded again
