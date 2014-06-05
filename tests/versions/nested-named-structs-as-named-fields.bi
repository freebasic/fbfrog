#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: ""1"", ""2"""
#endif

#pragma once

type __freebasic_dummyid_0
	#if __VERSION__ = "1"
		v1_a as long
	#else
		v2_a as long
	#endif
end type

#if __VERSION__ = "1"
	type __freebasic_dummyid_1
		v1_b as long
	end type
#endif

type UDT
	a as __freebasic_dummyid_0

	#if __VERSION__ = "1"
		b as __freebasic_dummyid_1
	#endif
end type
