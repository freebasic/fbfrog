#pragma once

' @fbfrog -v
' should become
'    typedef struct a ## b;
' not
'    typedef struct ab;
#define m(param) param
'' TODO: typedef struct a ## b;
