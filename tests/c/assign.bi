#pragma once

extern "C"

'' TODO: static int i1 = i1 = 0;

private sub f1()
	dim a as long
	a = 1
	'' TODO: a = a = 2;
end sub

if 1 then
	a = 1
end if

if 1 then
	a = 1
end if

#define M1(x) scope : x = 1 : end scope
#define M2(x) scope : (x) = 1 : end scope
'' TODO: #define M3(x) ((x) = (x) = 1)
#macro M4
	if 1 then
		a = 1
	end if
#endmacro
#macro M5
	if 1 then
		a = 1
	end if
#endmacro
#macro M6
	if 1 then
		a = 1
	end if
#endmacro
#macro M7
	if 1 then
		a = 1
	end if
#endmacro
scope
	a = 1
	a or= 1
	a xor= 1
	a and= 1
	a shl= 1
	a shr= 1
	a += 1
	a -= 1
	a *= 1
	a /= 1
	a mod= 1
end scope
#define ASSIGN_01 scope : a = 1 : end scope
#define ASSIGN_02 scope : a or= 1 : end scope
#define ASSIGN_03 scope : a xor= 1 : end scope
#define ASSIGN_04 scope : a and= 1 : end scope
#define ASSIGN_05 scope : a shl= 1 : end scope
#define ASSIGN_06 scope : a shr= 1 : end scope
#define ASSIGN_07 scope : a += 1 : end scope
#define ASSIGN_08 scope : a -= 1 : end scope
#define ASSIGN_09 scope : a *= 1 : end scope
#define ASSIGN_10 scope : a /= 1 : end scope
#define ASSIGN_11 scope : a mod= 1 : end scope

end extern
