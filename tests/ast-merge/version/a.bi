#ifndef __TEST_VERSION__
	#define __TEST_VERSION__ 1
#endif
#if __TEST_VERSION__ <> 1
	#error "'__TEST_VERSION__' is #defined to an unsupported value; expected one of: 1"
#endif
dim shared common as long
dim shared v1 as long

type UDTv1
	fieldv1 as long
end type

type UDTv1
	fieldv1 as long
end type

type UDTv1
	fieldv1 as long
end type
