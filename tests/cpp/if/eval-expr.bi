extern "C"

'' @fbfrog -whitespace -nonamefixup

extern test_ifdef_abc_expect_no as short

extern test_zero_andalso_defined_foo_expect_no as short

extern test_one_orelse_defined_foo_expect_yes as short
extern yes as short

extern test_one_plus_two_equals_three_expect_yes as short
extern yes as short

extern test_ifdef_FOO_known_defined_expect_yes as short
extern yes as short

extern test_ifdef_FOO_known_undefined_expect_no as short

extern test_ifdef_A_known_defined_andalso_macro_expanded_one_expect_yes as short
#define A
const B = 1
extern yes as short

extern test_if_defined_C_andalso_C_equals_123_expect_no as short

extern test_if_defined_D_andalso_D_equals_123_expect_yes as short
const D = 123
extern yes as short

end extern
