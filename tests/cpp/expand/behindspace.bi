#pragma once

' @fbfrog -removedefine mergetest
#define stringify1(s) #s
#define stringify2(s) stringify1(s)
dim shared test1 as zstring ptr = "mergetest(x, y)" ' "mergetest(x, y)"
dim shared test2 as zstring ptr = "mergetest( x, y)" ' "mergetest( x, y)"
dim shared test3 as zstring ptr = "prefix xy" ' "prefix xy"
dim shared test4 as zstring ptr = "prefix xy" ' "prefix xy"
#define TEST_MERGE x##y
dim shared test5 as zstring ptr = "xy" ' "xy"
dim shared test6 as zstring ptr = "prefix xy" ' "prefix xy"
dim shared test7 as zstring ptr = "=xy" ' "=xy"
dim shared test8 as zstring ptr = "= xy" ' "= xy"
' Macro whose first token isn't behind space
#define m10() x
dim shared test10 as zstring ptr = "=x" ' "=x"
dim shared test11 as zstring ptr = "= x" ' "= x"
' Macro whose first token is a param, and it's behind space
#define m20(a) a
dim shared test20 as zstring ptr = "=0" ' "=0"
dim shared test21 as zstring ptr = "= 0" ' "= 0"
dim shared test22 as zstring ptr = "==" ' "=="
dim shared test23 as zstring ptr = "= =" ' "= ="
' Macro whose first token is a param, and it isn't behind space
#define m30(a) a
dim shared test30 as zstring ptr = "=0" ' "=0"
dim shared test31 as zstring ptr = "= 0" ' "= 0"
dim shared test32 as zstring ptr = "==" ' "=="
dim shared test33 as zstring ptr = "= =" ' "= ="
