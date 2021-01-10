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

scope
	dim m as StringMatcher
	m.addPattern("LPCSTR")
	m.addPattern("LPSTR")
	m.addPattern("ListLabel")

	test(m.matches("LPCSTR"))
	test(m.matches("LPSTR"))
	test(m.matches("ListLabel"))
	test(not m.matches("LPCWSTR"))
end scope

scope
	dim m as StringMatcher
	m.addPattern("aa")
	m.addPattern("a")

	test(m.matches("a"))
	test(m.matches("aa"))
end scope

scope
	dim m as StringMatcher
	m.addPattern("1", cptr(any ptr, 1))
	m.addPattern("2", cptr(any ptr, 2))
	m.addPattern("3", cptr(any ptr, 3))
	m.addPattern("22", cptr(any ptr, 22))

	dim payload as any ptr

	'' should return the proper payloads
	payload = 0 : test(m.matches("1", payload)) : test(cint(payload) = 1)
	payload = 0 : test(m.matches("2", payload)) : test(cint(payload) = 2)
	payload = 0 : test(m.matches("3", payload)) : test(cint(payload) = 3)
	payload = 0 : test(m.matches("22", payload)) : test(cint(payload) = 22)

	'' should overwrite 3's payload (and nothing else)
	m.addPattern("3", cptr(any ptr, 333))
	payload = 0 : test(m.matches("1", payload)) : test(cint(payload) = 1)
	payload = 0 : test(m.matches("2", payload)) : test(cint(payload) = 2)
	payload = 0 : test(m.matches("3", payload)) : test(cint(payload) = 333)
	payload = 0 : test(m.matches("22", payload)) : test(cint(payload) = 22)
end scope
