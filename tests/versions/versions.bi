#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if ((__VERSION__ <> "1") and (__VERSION__ <> "2")) and (__VERSION__ <> "3")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"", ""2"", ""3"""
#endif

dim shared common_ as long

#if __VERSION__ = "1"
	dim shared v1 as long

	type UDTv1
		fieldv1 as long
	end type
#elseif __VERSION__ = "2"
	dim shared v2 as long

	type UDTv2
		fieldv2 as long
	end type
#endif

#if (__VERSION__ = "1") or (__VERSION__ = "2")
	dim shared v12 as long

	type UDT1v12
		fieldv12 as long
	end type

	type UDT2v12
		fieldv1 as long
		fieldv2 as long
	end type
#else
	dim shared v3 as long

	type UDTv3
		fieldv3 as long
	end type
#endif

type UDT1v123
	fieldv123 as long
end type

type UDT2v123
	#if __VERSION__ = "1"
		fieldv1 as long
	#elseif __VERSION__ = "2"
		fieldv2 as long
	#else
		fieldv3 as long
	#endif
end type
