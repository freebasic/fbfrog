#pragma once

' @fbfrog -v
' GCC's "({...})" statement-expressions can't be translated to FB
'' TODO: #define A1 ({ int i = foo(0); i; })
#define A2 scope : /' TODO: int a[({ f(); })]; '/ : end scope
