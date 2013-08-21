#ifndef TEST_H
#define TEST_H

#define A 1
#define B 2
#define C 3

#if (iif( defined( FOO_BAR ), 0, 1 ) andalso (THIS_IS_INSANE >= 123)) orelse ((OH_MAN_WHATS_THE_PRECEDENCE < 5) andalso (defined( OK ) orelse defined( I_DONT_KNOW )))
	#define PPMERGE( a, b ) a##b
	#define PPSTRINGIZE( a ) #a

	#if (X = 4294967295) orelse (X = (-1))
		#define HOORAY
	#endif
#elseif 1
	'' TODO: unknown PP directive
	#pragma foo
#endif

#define myDoSomething( a, b, c ) doSomething(1, c, b, a, 0)

#endif
