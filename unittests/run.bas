#include once "../src/source.bi"
#include once "../src/util-hash.bi"
#include once "../src/util-path.bi"
#include once "../src/util-str.bi"

#macro TEST(x)
	scope
		__ctx__.tests += 1
		if (x) = 0 then
			print __FILE__ + "(" & __LINE__ & "): failed: " + #x
			__ctx__.failures += 1
		end if
	end scope
#endmacro

#macro TESTEQ(a, b)
	scope
		__ctx__.tests += 1
		if (a) <> (b) then
			print __FILE__ + "(" & __LINE__ & "): failed:"
			print !"\texpected: " + #a + " = " + #b
			print !"\t but got: '" + str(a) + "' <> '" + str(b) + "'"
			__ctx__.failures += 1
		end if
	end scope
#endmacro

type UnitTestRunnerContext
	tests as integer
	failures as integer
end type

dim __ctx__ as UnitTestRunnerContext

scope
	#include once "test-source.bi"
end scope

scope
	#include once "test-util-hash.bi"
end scope

scope
	#include once "test-util-path.bi"
end scope

scope
	#include once "test-util-str-matching.bi"
end scope

scope
	#include once "test-util-str.bi"
end scope

var successes = __ctx__.tests - __ctx__.failures
print successes & "/" & __ctx__.tests & " tests successful, " & __ctx__.failures & " failed"
if __ctx__.failures > 0 then
	print "result: failed"
	end 1
end if
print "result: ok"
