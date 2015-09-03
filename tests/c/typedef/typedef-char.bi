#pragma once

extern "C"

type CHAR as zstring
type WCHAR as wstring
type CCHAR as const zstring
type CWCHAR as const wstring

extern p1 as CHAR ptr
extern p2 as WCHAR ptr
extern p3 as CHAR const ptr
extern p4 as WCHAR const ptr
extern p5 as const CHAR ptr
extern p6 as const WCHAR ptr
extern p7 as const CHAR ptr
extern p8 as const WCHAR ptr
extern p10 as CCHAR ptr
extern p20 as CWCHAR ptr
extern p30 as CCHAR const ptr
extern p40 as CWCHAR const ptr
extern p50 as const CCHAR ptr
extern p60 as const CWCHAR ptr
extern p70 as const CCHAR ptr
extern p80 as const CWCHAR ptr
extern c1 as byte
extern c2 as wchar_t
extern c3 as const byte
extern c4 as const wchar_t
extern c10 as const byte
extern c20 as const wchar_t
extern c30 as const byte
extern c40 as const wchar_t
extern array1 as zstring * 10
extern array2 as wstring * 10
extern array3 as const zstring * 10
extern array4 as const wstring * 10
extern array10 as const zstring * 10
extern array20 as const wstring * 10
extern array30 as const zstring * 10
extern array40 as const wstring * 10

end extern
