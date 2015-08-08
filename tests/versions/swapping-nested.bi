#pragma once

#ifdef __FB_64BIT__
	type UDT field = 1
		#ifdef __FB_64BIT__
			#ifdef UNICODE
				unicode as long
			#else
				ansi as long
			#endif
		#endif
	end type
#else
	type UDT
		#ifndef __FB_64BIT__
			#ifdef UNICODE
				unicode as long
			#else
				ansi as long
			#endif
		#endif
	end type
#endif
