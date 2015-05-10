#pragma once

if 1 then
	printf("true")
end if
if 1 then
	printf("true")
else
	printf("false")
end if
if 1 then
	scope
		printf("true")
	end scope
else
	scope
		printf("false")
	end scope
end if
if 1 then
end if
if 1 then
	printf("true")
end if
if 1 then
	printf("true")
else
	scope
		printf("false")
	end scope
end if
if 1 then
	scope
		printf("true")
	end scope
else
	printf("false")
end if
if 1 then
	scope
		printf("true")
		printf("true")
		printf("true")
	end scope
else
	scope
		printf("false")
		printf("false")
		printf("false")
	end scope
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
