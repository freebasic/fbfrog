#pragma once

#if defined(__FB_WIN32__) or defined(__FB_UNIX__)
	extern "C"
#endif

dim shared varall as long

#ifdef __FB_LINUX__
	dim shared varlinux as long

	type UDTlinux
		fieldlinux as long
	end type
#elseif defined(__FB_DOS__)
	dim shared vardos as long

	type UDTdos
		fielddos as long
	end type
#endif

#if defined(__FB_DOS__) or defined(__FB_LINUX__)
	dim shared vardoslinux as long

	type UDTdoslinux1
		fielddoslinux as long
	end type

	type UDTdoslinux2
		#ifdef __FB_LINUX__
			fieldlinux as long
		#elseif defined(__FB_DOS__)
			fielddos as long
		#endif
	end type
#elseif defined(__FB_WIN32__)
	dim shared varwin32 as long

	type UDTwin32
		fieldwin32 as long
	end type
#endif

type UDTall
	fieldall as long
end type

type UDTall2
	#ifdef __FB_LINUX__
		fieldlinux as long
	#elseif defined(__FB_WIN32__)
		fieldwin32 as long
	#elseif defined(__FB_DOS__)
		fielddos as long
	#endif
end type

#if defined(__FB_WIN32__) or defined(__FB_UNIX__)
	extern nondos as long
#endif

#if defined(__FB_64BIT__) and (defined(__FB_WIN32__) or defined(__FB_UNIX__))
	extern _64bit as long
#endif

#if defined(__FB_WIN32__) or defined(__FB_UNIX__)
	end extern
#endif
