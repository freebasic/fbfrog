#pragma once

#include once "foo/1.bi"
#include once "1/2.bi"

' Macro expansion should not be done inside the <...> part of an #include
const foo = 1
' foo/1.h
' unless it's a computed #include, then all tokens are expanded, even inside
' the <...>
'' TODO: #define filename <foo/2.h>
' 1/2.h
