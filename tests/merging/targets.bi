#pragma once

dim shared doslinuxwin32 as long

#ifdef __FB_WIN32__
	dim shared win32 as long

	type UDTwin32
		fieldwin32 as long
	end type
#elseif defined(__FB_LINUX__)
	dim shared linux as long

	type UDTlinux
		fieldlinux as long
	end type
#else
	dim shared dos as long

	type UDTdos
		fielddos as long
	end type
#endif

#if defined(__FB_DOS__) or defined(__FB_LINUX__)
	dim shared doslinux as long

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
#endif

type UDTdoslinuxwin32
	fielddoslinuxwin32 as long
end type

type UDTfielddoslinuxwin32
	#ifdef __FB_WIN32__
		fieldwin32 as long
	#elseif defined(__FB_LINUX__)
		fieldlinux as long
	#else
		fielddos as long
	#endif
end type
