#pragma once

extern "C"

type int as zstring
extern i as byte
#undef int
type define as long
extern i as long
#undef define
'' TODO: #define m1(int, define, defined) int define defined
declare sub f()
#undef m1
#define int
declare sub ok()
declare sub ok()
declare sub ok()
declare sub ok()
'' TODO: #define defined_int_1 defined int
declare sub ok()
#undef defined_int_1
'' TODO: #define defined_int_2 defined(int)
declare sub ok()
#undef defined_int_2
#undef int
declare sub ok()
declare sub ok()
declare sub ok()
declare sub ok()
'' TODO: #define defined_long_1 defined long
declare sub ok()
#undef defined_long_1
'' TODO: #define defined_long_2 defined(long)
declare sub ok()
#undef defined_long_2
#define define
declare sub ok()
declare sub ok()
declare sub ok()
declare sub ok()
#undef define
declare sub ok()
declare sub ok()
declare sub ok()
declare sub ok()

#define __attribute__
#define __restrict
#define __restrict__
#define auto
#define break
#define case
#define char
#define const
#define continue
#define default
#define define
#define do
#define double
#define elif
#define else
#define endif
#define enum
#define error
#define extern
#define float
#define for
#define goto
#define if
#define ifdef
#define ifndef
#define include
#define inline
#define int
#define long
#define pragma
#define register
#define restrict
#define return
#define short
#define signed
#define sizeof
#define static
#define struct
#define switch
#define typedef
#define undef
#define union
#define unsigned
#define void
#define volatile
#define warning
#define while

end extern
