#ifndef TEST_H
#define TEST_H

#define A               1
#define B               2
#define C _
                        3

/' PP expressions '/
#if (/' TODO: token 18 '/defined(FOO_BAR) andalso THIS_IS_INSANE >= 123) _
    orelse (OH_MAN_WHATS_THE_PRECEDENCE < 5 andalso (defined(OK) _
                                            orelse defined(I_DONT_KNOW)))
	#define PPMERGE(a, b) a##b
	#define PPSTRINGIZE(a) #a

#	if X = 4294967295UL orelse X = 0.1e+1
#		define HOORAY
#	endif
#endif

#endif
