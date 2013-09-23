dim shared i as long
#if (major = 1) and ((minor = 0) or (minor = 99))
	dim shared major1 as long
#endif
#if (major = 2) and ((minor = 0) or (minor = 99))
	dim shared major2 as long
#endif
#if (major = 3) and ((minor = 0) or (minor = 99))
	dim shared major3 as long
#endif
#if (major = 4) and ((minor = 0) or (minor = 99))
	dim shared major4 as long
#endif
#if (major = 5) and ((minor = 0) or (minor = 99))
	dim shared major5 as long
#endif
#if (major = 6) and ((minor = 0) or (minor = 99))
	dim shared major6 as long
#endif
#if (((((major = 1) or (major = 2)) or (major = 3)) or (major = 4)) or (major = 5)) or (major = 6)
	#if minor = 0
		dim shared minor0 as long
	#endif
	#if minor = 99
		dim shared minor99 as long
	#endif
#endif
#if (major = 3) and (minor = 1)
	dim shared major3 as long
#endif
#if (major = 4) and (minor = 1)
	dim shared major4 as long
#endif
#if (major = 5) and ((minor = 1) or (minor = 2))
	dim shared major5 as long
#endif
#if (major = 6) and (((minor = 1) or (minor = 2)) or (minor = 3))
	dim shared major6 as long
#endif
#if ((((major = 3) or (major = 4)) or (major = 5)) or (major = 6)) and (minor = 1)
	dim shared minor1 as long
#endif
#if ((major = 5) or (major = 6)) and (minor = 2)
	dim shared minor2 as long
#endif
#if (major = 6) and (minor = 3)
	dim shared minor3 as long
#endif
type UDT
	#if (major = 1) and ((minor = 0) or (minor = 99))
		fieldmajor1 as long
	#endif
	#if (major = 2) and ((minor = 0) or (minor = 99))
		fieldmajor2 as long
	#endif
	#if (((major = 3) or (major = 4)) and (((minor = 1) or (minor = 0)) or (minor = 99))) or (((major = 5) and ((((minor = 1) or (minor = 2)) or (minor = 0)) or (minor = 99))) or ((major = 6) and (((((minor = 1) or (minor = 2)) or (minor = 3)) or (minor = 0)) or (minor = 99))))
		fieldmajor3456 as long
	#endif
end type
