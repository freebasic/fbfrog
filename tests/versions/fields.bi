#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"", ""2"""
#endif

#pragma once

type UDT1
	field1 as long
	field2 as long
end type

type UDT2
	field1 as long

	#if __VERSION__ = "1"
		field2 as long
	#else
		field3 as long
	#endif

	field4 as long
end type
