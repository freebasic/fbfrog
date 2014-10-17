// @fbfrog -filterout '*'
// callconv.h includes 3 cdecl functions, but they will be filtered out, so
// here in this file we should still prefer to use an EXTERN block that covers
// the stdcall function from here.

#include "callconv.h"

__attribute__((stdcall)) void f4(void);
