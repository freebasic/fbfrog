#pragma once

#include once "crt/long.bi"
#include once "crt/longdouble.bi"

extern "C"

dim shared a as long
dim shared a as ulong
dim shared a as byte
dim shared a as byte
dim shared a as ubyte
dim shared a as short
dim shared a as short
dim shared a as ushort
dim shared a as short
dim shared a as short
dim shared a as ushort
dim shared a as long
dim shared a as long
dim shared a as ulong
dim shared a as clong
dim shared a as clong
dim shared a as culong
dim shared a as clong
dim shared a as clong
dim shared a as culong
dim shared a as longint
dim shared a as longint
dim shared a as ulongint
dim shared a as longint
dim shared a as longint
dim shared a as ulongint
dim shared a as single
dim shared a as double
dim shared a as clongdouble
dim shared a as clongdouble
dim shared a as byte
dim shared a as short
dim shared a as long
dim shared a as longint
dim shared a as byte
dim shared a as ubyte
dim shared a as short
dim shared a as ushort
dim shared a as long
dim shared a as ulong
dim shared a as longint
dim shared a as ulongint
dim shared a as uinteger
dim shared a as integer
dim shared a as integer
dim shared a as integer
dim shared a as uinteger
dim shared a as wchar_t
dim shared a as wstring ptr
dim shared a as byte
dim shared a as A
dim shared a as A
dim shared a as A
dim shared a as A
dim shared a as zstring ptr
dim shared a as const zstring ptr
dim shared a as const ubyte ptr
dim shared a as long ptr ptr ptr ptr
dim shared a as long ptr
dim shared b as long ptr ptr
dim shared c as long
dim shared d as long ptr ptr ptr ptr
dim shared a as short
dim shared a as const ulongint
dim shared a as long const ptr
dim shared b as long ptr
dim shared c as long ptr ptr const ptr ptr
dim shared a as const long
dim shared x as UDT ptr
dim shared a as long ptr ptr
dim shared a as long ptr const ptr
dim shared a as long const ptr ptr
dim shared a as long const ptr const ptr
dim shared a as const long ptr ptr
dim shared a as const long ptr const ptr
dim shared a as const long const ptr ptr
dim shared a as const long const ptr const ptr
dim shared a as const long ptr ptr
dim shared a as const long ptr const ptr
dim shared a as const long const ptr ptr
dim shared a as const long const ptr const ptr
dim shared a as const long ptr ptr
dim shared a as const long ptr const ptr
dim shared a as const long const ptr ptr
dim shared a as const long const ptr const ptr
dim shared p as long ptr
dim shared p as long ptr
dim shared p as long ptr
dim shared p as long ptr ptr
dim shared p as long ptr ptr
dim shared p as long ptr ptr
dim shared p as long const ptr const ptr
dim shared p as long const ptr const ptr
dim shared x as jmp_buf
dim shared px as jmp_buf ptr
dim shared cx as const jmp_buf
dim shared pcx as const jmp_buf ptr

declare sub f(byval x as jmp_buf ptr)
declare sub f(byval px as jmp_buf ptr)
declare sub f(byval cx as const jmp_buf ptr)
declare sub f(byval pcx as const jmp_buf ptr)

end extern
