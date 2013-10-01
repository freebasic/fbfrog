dim shared common as long
#if __TEST_VERSION__ = 1
	dim shared v1 as long
	type UDTv1
		fieldv1 as long
	end type
#elseif __TEST_VERSION__ = 2
	dim shared v2 as long
	type UDTv2
		fieldv2 as long
	end type
#endif
#if (__TEST_VERSION__ = 1) or (__TEST_VERSION__ = 2)
	dim shared v12 as long
	type UDTv12
		fieldv12 as long
	end type
	type UDTv12
		fieldv1 as long
		fieldv2 as long
	end type
#else
	dim shared v3 as long
	type UDTv3
		fieldv3 as long
	end type
#endif
type UDTv123
	fieldv123 as long
end type
type UDTv123
	#if __TEST_VERSION__ = 1
		fieldv1 as long
	#elseif __TEST_VERSION__ = 2
		fieldv2 as long
	#else
		fieldv3 as long
	#endif
end type
