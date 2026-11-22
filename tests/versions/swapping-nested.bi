#pragma once

' @fbfrog -declarebool UNICODE -ifdef UNICODE -define UNICODE 1 -endif -target windows
' When swapping verblocks (or really not only then, but in general) we shouldn't
' forget about processing nested verblocks.

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
