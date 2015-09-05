#pragma once

extern "C"

if 1 then
	printf("true")
end if

if 1 then
	printf("true")
else
	printf("false")
end if

if 1 then
	printf("true")
else
	printf("false")
end if

if 1 then
else
end if

if 1 then
	printf("true")
else
end if

if 1 then
	printf("true")
else
	printf("false")
end if

if 1 then
	printf("true")
else
	printf("false")
end if

if 1 then
	printf("true")
	printf("true")
	printf("true")
else
	printf("false")
	printf("false")
	printf("false")
end if

scope
	if 1 then
		printf("true")
	end if

	printf("1")
	printf("2")

	if 1 then
	else
		printf("false")
	end if

	printf("1")
	printf("2")
end scope

if "1" then
	if "1+.1" then
		printf("1+.1+")
	else
		printf("1+.1-")
	end if

	if "1+.2" then
		printf("1+.2+")
	else
		printf("1+.2-")
	end if
else
	if "1-.1" then
		printf("1-.1+")
	else
		printf("1-.1-")
	end if

	if "1-.2" then
		printf("1-.2+")
	else
		printf("1-.2-")
	end if
end if

private sub f()
	if "1" then
		if "1+.1" then
			printf("1+.1+")
		else
			printf("1+.1-")
		end if
		if "1+.2" then
			printf("1+.2+")
		else
			printf("1+.2-")
		end if
	else
		if "1-.1" then
			printf("1-.1+")
		else
			printf("1-.1-")
		end if
		if "1-.2" then
			printf("1-.2+")
		else
			printf("1-.2-")
		end if
	end if
end sub

if 1 = 2 then
end if

if 1 then
	printf("1")
elseif 2 then
	printf("2")
else
	printf("3")
end if

if 1 then
	printf("1")
elseif 2 then
	printf("2")
elseif 3 then
	printf("3")
elseif 4 then
	printf("4")
else
	printf("5")
end if

if 1 then
	printf("1")
elseif 2 then
	printf("2")
end if

if 1 then
elseif 2 then
end if

if 1 then
elseif 2 then
else
end if

if 1 then
	printf("1")
elseif 2 then
	printf("2")
else
	printf("3")
end if

if 1 then
	printf("1")
elseif 2 then
	printf("2.1")
else
	printf("2.2")
end if

if a then
	b
end if

#macro M1
	if 1 then
	else
	end if
#endmacro

end extern
