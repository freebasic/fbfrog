// @fbfrog <dir>simplifications.fbfrog

#if V == 1
	// An #if block should be generated for these, since their presence depends on the value of V.
	#define exists_only_in_V1
#else
	#define exists_only_in_V2
#endif

// No #if block should be generated for this, since it's present in all APIs
#define exists_in_all_versions

#ifndef _WIN64
	// For this, an #if block should be generated, but the condition should not check V,
	// since this is present for all V values.
	#define exists_not_for_win64
#endif
