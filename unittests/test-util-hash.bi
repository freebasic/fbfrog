scope
	dim t as THash = THash(2, TRUE)
	'TEST(t.lookup("a", hashHash("a")) = NULL)
	TEST(not t.contains("a", hashHash("a")))
	TEST(t.lookupDataOrNull("a") = NULL)
	TEST(t.lookupDataOrNull("b") = NULL)
	TEST(t.lookupDataOrNull("aa") = NULL)
	TESTEQ(t.count, 0)

	t.addOverwrite("a", cptr(any ptr, 123))
	scope
		var item = t.lookup("a", hashHash("a"))
		TEST(item <> NULL)
		TEST(item->data = cptr(any ptr, 123))
	end scope
	TEST(t.contains("a", hashHash("a")) <> FALSE)
	TEST(t.lookupDataOrNull("a") = cptr(any ptr, 123))
	TEST(t.lookupDataOrNull("b") = NULL)
	TEST(t.lookupDataOrNull("aa") = NULL)
	TESTEQ(t.count, 1)

	t.addOverwrite("b", cptr(any ptr, 456))
	TEST(t.lookupDataOrNull("a") = cptr(any ptr, 123))
	TEST(t.lookupDataOrNull("b") = cptr(any ptr, 456))
	TEST(t.lookupDataOrNull("aa") = NULL)
	TESTEQ(t.count, 2)

	t.addOverwrite("c", cptr(any ptr, 789))
	TEST(t.lookupDataOrNull("a") = cptr(any ptr, 123))
	TEST(t.lookupDataOrNull("b") = cptr(any ptr, 456))
	TEST(t.lookupDataOrNull("c") = cptr(any ptr, 789))
	TEST(t.lookupDataOrNull("aa") = NULL)
	TESTEQ(t.count, 3)

	t.addOverwrite("aa", cptr(any ptr, 111))
	TEST(t.lookupDataOrNull("a") = cptr(any ptr, 123))
	TEST(t.lookupDataOrNull("b") = cptr(any ptr, 456))
	TEST(t.lookupDataOrNull("c") = cptr(any ptr, 789))
	TEST(t.lookupDataOrNull("aa") = cptr(any ptr, 111))
	TESTEQ(t.count, 4)

	'' Overwriting
	t.addOverwrite("aa", cptr(any ptr, 222))
	TEST(t.lookupDataOrNull("a") = cptr(any ptr, 123))
	TEST(t.lookupDataOrNull("b") = cptr(any ptr, 456))
	TEST(t.lookupDataOrNull("c") = cptr(any ptr, 789))
	TEST(t.lookupDataOrNull("aa") = cptr(any ptr, 222))
	TESTEQ(t.count, 4)
end scope

'' Hash table entries should be preserved when the table is reallocated
scope
	dim t as THash = THash(0, TRUE)
	TEST(t.lookupDataOrNull("a") = NULL)
	TESTEQ(t.count, 0)

	const N = 100

	for i as integer = 0 to N - 1
		t.addOverwrite("foo" & i, cptr(any ptr, i))
		TESTEQ(t.count, i + 1)
		for j as integer = 0 to i
			TEST(t.lookupDataOrNull("foo" & j) = cptr(any ptr, j))
		next
	next

	TESTEQ(t.count, N)
	for i as integer = 0 to N - 1
		TEST(t.lookupDataOrNull("foo" & i) = cptr(any ptr, i))
	next

	TEST(t.lookupDataOrNull("foo") = NULL)
	TEST(t.lookupDataOrNull("a") = NULL)
end scope
