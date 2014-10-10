#pragma once

#ifndef VER
	#define VER 3
#endif

#if VER = 1
#elseif VER = 2
#elseif VER = 3
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2, 3"
#endif

'' The following symbols have been renamed:
''     variable common => common_

dim shared common_ as long

#if VER = 1
	dim shared v1 as long

	type UDTv1
		fieldv1 as long
	end type
#elseif VER = 2
	dim shared v2 as long

	type UDTv2
		fieldv2 as long
	end type
#endif

#if (VER = 1) or (VER = 2)
	dim shared v12 as long

	type UDT1v12
		fieldv12 as long
	end type

	type UDT2v12
		#if VER = 1
			fieldv1 as long
		#elseif VER = 2
			fieldv2 as long
		#endif
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
	#if VER = 1
		fieldv1 as long
	#elseif VER = 2
		fieldv2 as long
	#else
		fieldv3 as long
	#endif
end type
