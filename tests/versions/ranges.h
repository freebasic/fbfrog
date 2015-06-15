// @fbfrog <dir>ranges.fbfrog

// Testing with versions 10, 20, 30 etc. instead of something more common such
// as 1, 2, 3, in the hopes of finding bugs where the version numbers themselves
// are confused with array indices for the version number array.
// Afterall, we want to support version numbers like &h101, &h102, &h200 etc. too.

extern int separator1;
#if V == 10
	extern int _10;
#elif V == 20
	extern int _20;
#elif V == 50
	extern int _50;
#elif V == 80
	extern int _80;
#elif V == 90
	extern int _90;
#endif

extern int separator2;
#if V <= 20
	extern int _lt20;
#endif
#if V <= 30
	extern int _lt30;
#endif
#if V <= 40
	extern int _lt40;
#endif

extern int separator3;
#if V >= 60
	extern int _gt60;
#endif
#if V >= 70
	extern int _gt70;
#endif
#if V >= 80
	extern int _gt80;
#endif

// Single ranges
extern int separator4;
#if V >= 20 && V <= 80
	extern int _20to80;
#endif
#if V >= 40 && V <= 60
	extern int _40to60;
#endif

// Individual checks needed
extern int separator5;
#if V == 20 || V == 40
	extern int _20_40;
#endif
#if V == 20 || V == 50 || V == 80
	extern int _20_50_80;
#endif

// Multiple ranges
extern int separator6;
#if (V >= 20 && V <= 40) || (V >= 60 && V <= 80)
	extern int _20to40_60to80;
#endif

// 20..30 is just 2 versions, a range check isn't worth it here
#if (V >= 20 && V <= 30) || (V >= 60 && V <= 80)
	extern int _20to30_60to80;
#endif

// 10..20 is also just 2 versions, but 10 is the first version, so it can be optimized to just a V<=20 check
#if (V >= 10 && V <= 20) || (V >= 60 && V <= 80)
	extern int _10to20_60to80;
#endif

// 70..80 is just 2 versions, a range check isn't worth it here
#if (V >= 20 && V <= 40) || (V >= 70 && V <= 80)
	extern int _20to40_70to80;
#endif

// 80..90 is also just 2 versions, but 90 is the last version, so it can be optimized to just a V>=80 check
#if (V >= 20 && V <= 40) || (V >= 80 && V <= 90)
	extern int _20to40_80to90;
#endif
