#include "test"
#include "../../ast.bas"
#include "../../emit.bas"
#include "../../tk.bas"
#include "../../util.bas"
#include "../../util-hash.bas"
#include "../../util-path.bas"
#include "../../util-str.bas"

#macro dumpStringMatcherTree(patterns...)
	scope
		print string(80, "-")
		print "patterns: " + #patterns
		dim patternArray(0 to ...) as const string => { patterns }
		dim matcher as StringMatcher
		for i as integer = 0 to ubound(patternArray)
			matcher.addPattern(patternArray(i))
		next
		astDump(matcher.root)
	end scope
#endmacro

dumpStringMatcherTree("a", "b")
dumpStringMatcherTree("a", "b", "aa")
dumpStringMatcherTree("a", "b", "aa", "b*")
dumpStringMatcherTree("aa1", "aa2", "aa3")

scope
	dim m as StringMatcher
	m.addPattern("a")
	test(m.matches("a"))
	test(not m.matches("b"))
	test(not m.matches(""))
end scope

scope
	dim m as StringMatcher
	m.addPattern("a")
	m.addPattern("aa")
	m.addPattern("ba")
	m.addPattern("baa")

	test(m.matches("a"))
	test(m.matches("aa"))
	test(m.matches("ba"))
	test(m.matches("baa"))

	test(not m.matches("aaa"))
	test(not m.matches("aba"))
	test(not m.matches("ac"))
	test(not m.matches("b"))
	test(not m.matches("baaa"))
	test(not m.matches("bab"))
	test(not m.matches("bc"))
	test(not m.matches("c"))
	test(not m.matches(""))
end scope

scope
	dim m as StringMatcher
	m.addPattern("a*")

	test(m.matches("a"))
	test(m.matches("aa"))
	test(m.matches("ab"))
	test(m.matches("a1111111222223333"))

	test(not m.matches(""))
	test(not m.matches("b"))
	test(not m.matches("ba"))
end scope

scope
	dim m as StringMatcher
	m.addPattern("a*")
	m.addPattern("b*aa")

	test(m.matches("a"))
	test(m.matches("aa"))
	test(m.matches("aaaaaa"))
	test(m.matches("b11111aa"))
	test(m.matches("baa"))
	test(m.matches("baaa"))

	test(not m.matches(""))
	test(not m.matches("b"))
	test(not m.matches("ba"))
	test(not m.matches("caa"))
end scope

scope
	dim m as StringMatcher
	m.addPattern("a")
	m.addPattern("a*")
	m.addPattern("*a")

	test(m.matches("a"))
	test(m.matches("aa"))
	test(m.matches("ab"))
	test(m.matches("a1111111222223333"))
	test(m.matches("ba"))
	test(m.matches("baa"))
	test(m.matches("bbbbbbbbbbba"))

	test(not m.matches(""))
	test(not m.matches("b"))
	test(not m.matches("bbbbbbbb"))
end scope

scope
	dim m as StringMatcher
	m.addPattern("*a*b*")

	test(m.matches("ab"))
	test(m.matches("aabb"))
	test(m.matches("aaaabccccc"))
	test(m.matches("abccccccc"))
	test(m.matches("abbbbbbbbbc"))

	test(not m.matches(""))
	test(not m.matches("a"))
	test(not m.matches("aaaaaaaa"))
	test(not m.matches("b"))
	test(not m.matches("ba"))
	test(not m.matches("bbbbbbbb"))
	test(not m.matches("c"))
end scope
