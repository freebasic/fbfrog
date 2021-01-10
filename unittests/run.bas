#include once "../src/util-path.bi"
#include once "../src/util-str.bi"

#macro TEST(x)
	scope
		ctx.tests += 1
		if (x) = 0 then
			print __FILE__ + "(" & __LINE__ & "): failed: " + #x
			ctx.failures += 1
		end if
	end scope
#endmacro

#macro TESTEQ(a, b)
	scope
		ctx.tests += 1
		if (a) <> (b) then
			print __FILE__ + "(" & __LINE__ & "): failed:"
			print !"\texpected: " + #a + " = " + #b
			print !"\t but got: '" + str(a) + "' <> '" + str(b) + "'"
			ctx.failures += 1
		end if
	end scope
#endmacro

type UnitTestRunnerContext
	tests as integer
	failures as integer
end type

dim ctx as UnitTestRunnerContext

scope
	#include once "test-util-path.bi"
	#include once "test-util-str-matching.bi"
	#include once "test-util-str.bi"
end scope

var successes = ctx.tests - ctx.failures
print successes & "/" & ctx.tests & " tests successful, " & ctx.failures & " failed"
if ctx.failures > 0 then
	print "result: failed"
	end 1
end if
print "result: ok"
