#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if __VERSION__ <> "1"
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"""
#endif

dim shared common_ as long
dim shared v1 as long

type UDT1v1
	fieldv1 as long
end type

type UDT2v1
	fieldv1 as long
end type

type UDT3v1
	fieldv1 as long
end type
