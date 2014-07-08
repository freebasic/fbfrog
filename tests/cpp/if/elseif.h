// @fail
/* CPP only accepts #elif, not #elseif */
#if 1
#elseif 1
#else
#endif
