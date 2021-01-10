TEST(strStartsWith("", ""))
TEST(not strStartsWith("", "a"))
TEST(strStartsWith("a", ""))
TEST(strStartsWith("a", "a"))
TEST(strStartsWith("aa", "a"))
TEST(not strStartsWith("ba", "a"))

scope
	dim as string l, r
	strSplit("", "", l, r)
	TESTEQ(l, "")
	TESTEQ(r, "")
end scope

scope
	dim as string l, r
	strSplit("a,b", ",", l, r)
	TESTEQ(l, "a")
	TESTEQ(r, "b")
end scope

'' no delimiter in string
scope
	dim as string l, r
	strSplit("ab", ",", l, r)
	TESTEQ(l, "ab")
	TESTEQ(r, "")
end scope

'' in case of multiple delimiters, the first one is picked
scope
	dim as string l, r
	strSplit("a,b,c", ",", l, r)
	TESTEQ(l, "a")
	TESTEQ(r, "b,c")
end scope

'' multiple chars as delimiter
scope
	dim as string l, r
	strSplit("a,,b", ",,", l, r)
	TESTEQ(l, "a")
	TESTEQ(r, "b")
end scope

'' delimiter at start of string, FIXME: should give l="" r="a"
scope
	dim as string l, r
	strSplit(",a", ",", l, r)
	TESTEQ(l, ",a")
	TESTEQ(r, "")
end scope

'' delimiter at end of string
scope
	dim as string l, r
	strSplit("a,", ",", l, r)
	TESTEQ(l, "a")
	TESTEQ(r, "")
end scope

TESTEQ(strReplace("", "a", "b"), "") '' empty string
TESTEQ(strReplace("", "", ""), "") '' empty everything
TESTEQ(strReplace("a", "a", "a"), "a") '' no infinite recursion
TESTEQ(strReplace("a", "a", "aa"), "aa") '' no infinite recursion
TESTEQ(strReplace("a", "a", "b"), "b") '' simple replacement
TESTEQ(strReplace("c", "a", "b"), "c") '' no match
TESTEQ(strReplace("b", "a", "b"), "b") '' no match
TESTEQ(strReplace("a", "b", ""), "a") '' no match
TESTEQ(strReplace("a", "", "b"), "a") '' empty pattern
TESTEQ(strReplace("a", "a", ""), "") '' empty replacement = deletion
TESTEQ(strReplace("foo bar baz", "bar", "a"), "foo a baz") '' smaller replacement
TESTEQ(strReplace("foo bar baz", "bar", "aaaaaaa"), "foo aaaaaaa baz") '' bigger replacement
TESTEQ(strReplace("aaa", "a", "123"), "123123123") '' repeated bigger replacements

TESTEQ(strReplaceNonIdChars("", asc("?")), "")
TESTEQ(strReplaceNonIdChars("~?[]", asc("?")), "????")
TESTEQ(strReplaceNonIdChars("Foo=bar%%$!.h", asc("_")), "Foo_bar_____h")

TEST(strIsValidSymbolId("") = false)
TEST(strIsValidSymbolId("_"))
TEST(strIsValidSymbolId("____"))
TEST(strIsValidSymbolId("_a"))
TEST(strIsValidSymbolId("_A"))
TEST(strIsValidSymbolId("_1"))
TEST(strIsValidSymbolId("1") = false)
TEST(strIsValidSymbolId("a"))
TEST(strIsValidSymbolId("A"))
TEST(strIsValidSymbolId("abcABC_123"))

TEST(not strIsNumber(""))
TEST(strIsNumber("0"))
TEST(strIsNumber("9999"))
TEST(not strIsNumber("a"))
TEST(not strIsNumber("0xFF"))
TEST(not strIsNumber("&hFF"))

TEST(not strIsReservedIdInC(""))
TEST(not strIsReservedIdInC("f"))
TEST(not strIsReservedIdInC("F"))
TEST(not strIsReservedIdInC("_"))
TEST(not strIsReservedIdInC("foo"))
TEST(not strIsReservedIdInC("Foo"))
TEST(not strIsReservedIdInC("_foo"))
TEST(strIsReservedIdInC("__f"))
TEST(strIsReservedIdInC("__foo"))
TEST(strIsReservedIdInC("_F"))
TEST(strIsReservedIdInC("_Foo"))
