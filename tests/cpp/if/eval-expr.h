// @fbfrog -whitespace -nonamefixup

extern short test_ifdef_abc_expect_no;
#ifdef ABC
	extern short no;
#endif

extern short test_zero_andalso_defined_foo_expect_no;
#if 0 && defined foo
	extern short no;
#endif

extern short test_one_orelse_defined_foo_expect_yes;
#if 1 || defined foo
	extern short yes;
#endif

extern short test_one_plus_two_equals_three_expect_yes;
#if 1 + 2 == 3
	extern short yes;
#endif

extern short test_ifdef_FOO_known_defined_expect_yes;
#define FOO
#ifdef FOO
	extern short yes;
#endif

extern short test_ifdef_FOO_known_undefined_expect_no;
#undef FOO
#ifdef FOO
	extern short no;
#endif

extern short test_ifdef_A_known_defined_andalso_macro_expanded_one_expect_yes;
#define A
#define B 1
#if defined A && B
	extern short yes;
#endif

extern short test_if_defined_C_andalso_C_equals_123_expect_no;
#if defined C && C == 123
	extern short no;
#endif

extern short test_if_defined_D_andalso_D_equals_123_expect_yes;
#define D 123
#if defined D && D == 123
	extern short yes;
#endif
