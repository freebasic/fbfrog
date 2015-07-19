#pragma once

#ifdef __FB_64BIT__
	type UDT field = 1
		#if defined(__FB_64BIT__) and defined(UNICODE)
			unicode as long
		#elseif defined(__FB_64BIT__) and (not defined(UNICODE))
			ansi as long
		#endif
	end type
#else
	type UDT
		#if (not defined(__FB_64BIT__)) and defined(UNICODE)
			unicode as long
		#elseif (not defined(__FB_64BIT__)) and (not defined(UNICODE))
			ansi as long
		#endif
	end type
#endif
