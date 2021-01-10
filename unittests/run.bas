#include once "../src/util-str.bi"

#macro test(x)
	scope
		ctx.tests += 1
		if (x) = 0 then
			print __FILE__ + "(" & __LINE__ & "): failed: " + #x
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
