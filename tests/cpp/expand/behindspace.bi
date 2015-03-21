#pragma once

#define stringify1(s) #s
#define stringify2(s) stringify1(s)
dim shared test1 as zstring ptr = "mergetest(x, y)"
dim shared test2 as zstring ptr = "mergetest( x, y)"
dim shared test3 as zstring ptr = "prefix xy"
dim shared test4 as zstring ptr = "prefix xy"
#define TEST_MERGE x##y
dim shared test5 as zstring ptr = "xy"
dim shared test6 as zstring ptr = "prefix xy"
dim shared test7 as zstring ptr = "=xy"
dim shared test8 as zstring ptr = "= xy"
#define m10() x
dim shared test10 as zstring ptr = "=x"
dim shared test11 as zstring ptr = "= x"
#define m20(a) a
dim shared test20 as zstring ptr = "=0"
dim shared test21 as zstring ptr = "= 0"
dim shared test22 as zstring ptr = "=="
dim shared test23 as zstring ptr = "= ="
#define m30(a) a
dim shared test30 as zstring ptr = "=0"
dim shared test31 as zstring ptr = "= 0"
dim shared test32 as zstring ptr = "=="
dim shared test33 as zstring ptr = "= ="
