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
end if

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

end extern
