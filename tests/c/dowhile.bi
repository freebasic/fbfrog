#pragma once

do
loop while 1

do
	printf("1")
loop while 1

do
	printf("1")
loop while 1

do
	printf("1")
	printf("2")
	printf("3")
loop while 1

scope
	printf("1")
end scope

if 1 then
	printf("1")
end if

while 1
wend

while 1
	printf("1")
wend

while 1
	printf("1")
wend

while 1
	printf("1")
	printf("2")
	printf("3")
wend

scope
	printf("1")
end scope

if 1 then
	printf("1")
end if

#define M1 scope : printf("foo") : end scope
#macro M2
	do
		printf("foo")
	loop while 1
#endmacro
#define M3 scope : printf("foo") : end scope
#macro M4
	while 1
		printf("foo")
	wend
#endmacro

do
loop while a = b

while a = b
wend

while a
	b
wend
