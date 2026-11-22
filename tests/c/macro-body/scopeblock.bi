#pragma once

' Sometimes scope blocks don't even have ';' semi-colons
#macro M1
	if 1 then
	end if
#endmacro
#macro M2
	if 1 then
	end if
#endmacro
#macro M3
	do
	loop while 1
#endmacro
#macro M4
	do
	loop while 1
#endmacro
#macro M5
	while 1
	wend
#endmacro
#macro M6
	while 1
	wend
#endmacro
' 1. This macro body is a statement, and it's not a scope block, so fbfrog will
'    wrap it in a scope block
' 2. Highlevel transformations turn the do/while(0) into a scope block
' 3. Now there are two scope blocks that should both be removed since the
'    inner if block is good enough.
#macro A1
	if 1 then
		f(1)
	end if
#endmacro
