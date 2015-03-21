#pragma once

extern "C"

declare function f11(byval i as long) as long
declare function f12(byval i as long) as long
declare function f13(byval i as long) as long
declare function x1N(byval i as long) as long
declare sub f2 stdcall()
declare sub x2 stdcall()
declare sub f3(byval a as long, byval b as short, byval c as single, byval as double)
declare sub x3(byval a as long, byval b as short, byval c as single, byval as double)
declare function f4(byval i as long) as long
declare function x4(byval i as long) as long

dim shared p1 as function(byval i as long) as long
dim shared p2 as typeof(function(byval i as long) as long) ptr
dim shared p3 as typeof(function(byval i as long) as long) ptr ptr
type p4 as function(byval i as long) as long
declare sub p5(byval param as function(byval i as long) as long)
dim shared p6 as sub(byval param as function(byval i as long) as long)
declare sub p7(byval param as sub(byval innerparam as function(byval i as long) as long))
declare function p8() as function(byval i as long) as long
dim shared p9 as function() as function(byval i as long) as long
declare sub p10(byval param as function() as function(byval i as long) as long)
type p11 as function() as function(byval i as long) as long
#define p12(x) cptr(function cdecl(byval i as long) as long, x)
#define p13 sizeof(function cdecl(byval i as long) as long)
dim shared c1 as const function(byval i as long) as long
dim shared c2 as typeof(function(byval i as long) as long) const ptr
dim shared c3 as typeof(const function(byval i as long) as long) ptr
dim shared c4 as typeof(const function(byval i as long) as long) const ptr
declare function qualified1(byval i as long) as long
dim shared qualified2 as function(byval i as long) as long
dim shared qualified3 as const function(byval i as long) as long
declare sub f5(byval p as function(byval i as long) as long)

end extern
