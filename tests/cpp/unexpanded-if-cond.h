// @fail

// If foo() won't be expanded, the following will trigger a parser error.
// The error message should be good enough to show the problem (macro not
// expanded).

#if foo(1)
	#error
#endif
