#pragma once

extern "C"

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

extern separator4 as long

#if (V >= 20) and (V <= 80)
	extern _20to80 as long
#endif

#if (V >= 40) and (V <= 60)
	extern _40to60 as long
#endif

extern separator5 as long

#if (V = 20) or (V = 40)
	extern _20_40 as long
#endif

#if (V = 20) or (V = 50) or (V = 80)
	extern _20_50_80 as long
#endif

extern separator6 as long

#if ((V >= 20) and (V <= 40)) or ((V >= 60) and (V <= 80))
	extern _20to40_60to80 as long
#endif

#if (V = 20) or (V = 30) or ((V >= 60) and (V <= 80))
	extern _20to30_60to80 as long
#endif

#if (V <= 20) or ((V >= 60) and (V <= 80))
	extern _10to20_60to80 as long
#endif

#if ((V >= 20) and (V <= 40)) or (V = 70) or (V = 80)
	extern _20to40_70to80 as long
#endif

#if ((V >= 20) and (V <= 40)) or (V >= 80)
	extern _20to40_80to90 as long
#endif

end extern
