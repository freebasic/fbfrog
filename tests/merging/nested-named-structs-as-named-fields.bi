#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#if VER = 1
	type __dummyid_0_tests_merging_nested_named_structs_as_named_fields
		v1_a as long
	end type
#endif

type __dummyid_1_tests_merging_nested_named_structs_as_named_fields
	#if VER = 1
		v1_b as long
	#else
		v2_a as long
	#endif
end type

type UDT
	a as __dummyid_0_tests_merging_nested_named_structs_as_named_fields

	#if VER = 1
		b as __dummyid_1_tests_merging_nested_named_structs_as_named_fields
	#endif
end type
