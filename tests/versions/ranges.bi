#pragma once

extern "C"

' @fbfrog <dir>ranges.fbfrog
' Testing with versions 10, 20, 30 etc. instead of something more common such
' as 1, 2, 3, in the hopes of finding bugs where the version numbers themselves
' are confused with array indices for the version number array.
' Afterall, we want to support version numbers like &h101, &h102, &h200 etc. too.
extern separator1 as long

#if V = 10
	extern _10 as long
#elseif V = 20
	extern _20 as long
#elseif V = 50
	extern _50 as long
#elseif V = 80
	extern _80 as long
#elseif V = 90
	extern _90 as long
#endif

extern separator2 as long

#if V <= 20
	extern _lt20 as long
#endif

#if V <= 30
	extern _lt30 as long
#endif

#if V <= 40
	extern _lt40 as long
#endif

extern separator3 as long

#if V >= 60
	extern _gt60 as long
#endif

#if V >= 70
	extern _gt70 as long
#endif

#if V >= 80
	extern _gt80 as long
#endif

' Single ranges
extern separator4 as long

#if (V >= 20) and (V <= 80)
	extern _20to80 as long
#endif

#if (V >= 40) and (V <= 60)
	extern _40to60 as long
#endif

' Individual checks needed
extern separator5 as long

#if (V = 20) or (V = 40)
	extern _20_40 as long
#endif

#if (V = 20) or (V = 50) or (V = 80)
	extern _20_50_80 as long
#endif

' Multiple ranges
extern separator6 as long

#if ((V >= 20) and (V <= 40)) or ((V >= 60) and (V <= 80))
	extern _20to40_60to80 as long
#endif

' 20..30 is just 2 versions, a range check isn't worth it here

#if (V = 20) or (V = 30) or ((V >= 60) and (V <= 80))
	extern _20to30_60to80 as long
#endif

' 10..20 is also just 2 versions, but 10 is the first version, so it can be optimized to just a V<=20 check

#if (V <= 20) or ((V >= 60) and (V <= 80))
	extern _10to20_60to80 as long
#endif

' 70..80 is just 2 versions, a range check isn't worth it here

#if ((V >= 20) and (V <= 40)) or (V = 70) or (V = 80)
	extern _20to40_70to80 as long
#endif

' 80..90 is also just 2 versions, but 90 is the last version, so it can be optimized to just a V>=80 check

#if ((V >= 20) and (V <= 40)) or (V >= 80)
	extern _20to40_80to90 as long
#endif

end extern
