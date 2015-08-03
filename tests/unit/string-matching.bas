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
