// @fbfrog -convbodytokens A1 -convbodytokens A2 -convbodytokens A3

#define A1 docall(myfunction, (arg1, arg2))
#define A2 signed int i = (array[10 + x.p->offset(0xFF)])
#define A3(macroparam) \
	10, 010, 0x10, "a\\b", L"a\\b", '\\', L'\\', \
	~l					, \
	l == r					, \
	l != r					, \
	l + r					, \
	l - r					, \
	l * r					, \
	l / r					, \
	l % r					, \
	l | r					, \
	l & r					, \
	l ^ r					, \
	l << r					, \
	l >> r					, \
	l && r					, \
	l || r					, \
	l |= r					, \
	l %= r					, \
	l &= r					, \
	l <<= r					, \
	l >>= r					, \
	l ^= r					, \
	int eger, str##ing, #macroparam, #foo
