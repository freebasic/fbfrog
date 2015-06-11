#pragma once

dim shared common as long

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

#if VER = 3
	dim shared v3 as long

	type UDTv3
		fieldv3 as long
	end type
#else
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
