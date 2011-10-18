#ifndef TEST_H
#define TEST_H

#define A               1
#define B               2
#define C _
                        3

/' PP expressions '/
#if ( /'TODO: unary NOT'/ not defined(FOO_BAR) and THIS_IS_INSANE >= 123) _
    or (OH_MAN_WHATS_THE_PRECEDENCE < 5 and (defined(OK) _
                                            or defined(I_DONT_KNOW)))
	#define PPMERGE(a, b) a##b
	#define PPSTRINGIZE(a) #a

#	if X = 4294967295UL or X = 0.1e+1
#		define HOORAY
#	endif
#elseif 1
	/'TODO: #pragma'/
	#pragma foo
#endif

#endif
