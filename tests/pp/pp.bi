#ifndef TEST_H
#define TEST_H

#define A 1
#define B 2
#define C 3

'' TODO: unknown PP directive
#if (!defined(FOO_BAR) && THIS_IS_INSANE >= 123) \
    || (OH_MAN_WHATS_THE_PRECEDENCE < 5 && (defined(OK) \
                                            || defined(I_DONT_KNOW)))
#define PPMERGE (a, b) a##b
#define PPSTRINGIZE (a) #a

'' TODO: unknown PP directive
#if X == 4294967295UL || X == 0.1e+1
#define HOORAY
#endif
#elseif 1
'' TODO: unknown PP directive
#pragma foo

#endif

#endif
